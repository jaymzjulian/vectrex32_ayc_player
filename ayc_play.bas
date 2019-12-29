'---------------------------
' GLOBALS - these must be global 
' for your music to play!
' TODO: possibly refactor this to be a struct of an ayc_data type, so it can be passed
' instead of global
'---------------------------
'
' if this is 1, we call Sound, if this is 0, we call FillBuffer, and
' play via the codesprite
buffer_mode = 1

' do we wait for the next frame to be "due" before we continue?
buffer_mode_preserve_refresh = 0

' show the low framerate demo - this will show a set of sprites to demonstrate
' playing 50fps music when running at sub-25fps vectrex rounds
demo_mode = 1


dim comp_buffer[14]
dim cursor[14]
' our data file
dim ay_buffer_sizes[15]
dim ay_buffer_offsets[15]
' set  some player constants
dim last_flags[14]
dim comp_flags[14]
dim coffset[14]
dim poffset[14]
dim premaining[14]
dim ayc_duration
dim ay_buffer_data
dim ayc_duration
dim played_frames
dim ay_data_length
' drop this if you're only doing 2 channels, leaving one for sound effects
max_regs = 14

if buffer_mode = 1
  '--------------------------------------------------------------------'
  ' This is only required if buffer_mode is set to 1 - you can save a few byres of ram by excluding it if you want, otherwise
  ' number of buffers.  
  '
  ' Currently, we non-optionally consume 70 bytes of dpram per buffer - so 4 buffers would be 280 bytes of dpram. 
  buffer_count = 4
  ' rate of playback - 50hz by default...
  player_rate = 50
  ' This is almost certainly wrong/destructive!
  ' but is.... probably enough?
  dualport_return = 8
  dualport_status = 9
  ' you'll need to allow for max_regs*buffer_count worth of iram at this location 
  ' if this is the only weird thing you're using, c882 should be fine.  c880 is better, but doens't work
  ' on all v32 firmware revisions right now....
  buffer_location = $c882
  buffer_base = $c884
  ' buffers are 14*2 + 1 bytes long
  buffer_end = buffer_base + (buffer_count - 1) * 29

	game_frame_count = 0

  ' below here is not, for the most part, user servicable :)
  via_rate = 1500000 / player_rate 
  tick_rate = 960 / player_rate
  print "AYC: VIA Rate is "+via_rate+" cycles"
  print "AYC: Tick Rate is "+tick_rate+" cycles"
  current_buffer = 0
  ' we use this being negative to represent first frame
  ayc_buffer_played = -1
	ayc_dp_sequence = 254
  dim ayc_pokedata[max_regs*5*buffer_count]
  dim ay_output_data[buffer_count, max_regs*2]
  '--------------------------------------------------------------------'
  ' should be in hex format, but for now this is what we get!
  ' the listing for this is an other file within the github, but I built it with 
  ' asm80.com :)
  '--------------------------------------------------------------------'
  ' first line: check via
  ' second line: call sound_bytes_x, increment dualport return
  ' third line: write to VIA for next countdown timer
  ' fourth line: incremener buffer
  ayc_playcode = { _
		 $86, $20, $b5, $d0, $0d, $27, $21, _		 
     $FE, buffer_location / 256, buffer_location mod 256, $BD, $F2, $7D, $7c, dualport_return / 256, dualport_return mod 256, _
     $fc, via_rate mod 256, via_rate / 256, $fd, $d0, $08, _
     $fc, buffer_location / 256, buffer_location mod 256, $c3, $00, $1d, $10, $83, buffer_end / 256, buffer_end mod 256,  _
                        $2f, $03, $cc, buffer_base / 256, buffer_base mod 256, $fd, buffer_location / 256, buffer_location mod 256 _
      }

  '--------------------------------------------------------------------'
  ' this sets up the timer we need to keep time on the VX side...
  ' this will be modified by the player to set it to what's remaining for the first
  ' vblank that we need.  it should be called as early as posisble during your dualport config
  '
  ' It also resets the dualport_return register to 0 - that register ends up containing how many frames were acutally played!
  '--------------------------------------------------------------------'
	' 0000   CC 30 75               LDD   #$3075  ; this gets replaced by wait_time for first music call  
	' 0003   FD D0 08               STD   $d008   
	' 0006   86 00                  LDA   #0   
	' 0008   B7 01 23               STA   $123  ; this gets replaced with dualport_return
  ayc_init = { $cc, $30, $75, $fd, $d0, $08, $86, $00, $b7, dualport_return / 256, dualport_return mod 256 }

  '--------------------------------------------------------------------
  ' this resets the VIA at the end, so that wait_recal doens't wait - this should be the last thing you call.
	' note that, in VIA buffered mode, all it does is update dualport_status to the current sequence semaphore
	' without that, playback is.... weird due to locking...
  '--------------------------------------------------------------------
  ' first line is: lda #sequence, sta $dualport_status
	' second line is: ldd #$100, std $d008 (remember: endian is reversed)
	' which should set timer b to "almost nothing"
  ayc_exit = { $86, ayc_dp_sequence, $b7, dualport_status / 256, dualport_status mod 256, _
	  					 $cc, $1, $0, $fd, $d0, $08}

endif

' set the framerate to 50fps, since that is what most AYC tracks are
' in buffer mode, we set it just as fast as we can update it - our buffer code
' will actually cause this to be ignored anyhow....
if buffer_mode = 1 and buffer_mode_preserve_refresh = 0
  call SetFrameRate(150)
else
  call SetFrameRate(50)
endif
print GetFrameRate()

' load the AYC
call load_and_init("switchblade.ayc")

controls = WaitForFrame(JoystickNone, Controller1, JoystickNone)

' In buffer mode, ayc_startup must be the VERY FIRST codesprite called, else
' timing will be busted
if buffer_mode = 1
  call CodeSprite(ayc_init)
endif

' show some display.... obviously this is my tes stuff, you'd drop this for
' your own code, BUT...
if demo_mode = 1
  call ReturnToOriginSprite()
  call ScaleSprite(32)
  call MoveSprite(128, 200)
  call TextSprite("AYC PLAYER TEST")
  call ReturnToOriginSprite()
  call ScaleSprite(32)
  if buffer_mode = 1
    call rose()
  endif
endif

' ... this is the important stuff!!
if buffer_mode = 1
  ' This creates the codesprite objects that we need
  ' send to the AY if it's time
  ' 
  ' This needs to be called often enough to actually get useful timing...
  call CodeSprite(ayc_playcode)

  ' this needs to be the _very last thing) called, otherwise the VIA will just
  ' timeout - this is only required, of course, if we're preserving refresh
  call CodeSprite(ayc_exit)  
endif

ayc_start_time = GetTickCount()
while controls[1,3] = 0
  ' if you're using buffered mode, you should call this to update the initial timer
  ' vs the tick counter
	'
	' this should be called _directly before_ wait for frame, since it may block until the previous one
	' is (mostly) complete, specifically until ayc_exit is called - this is to ensure that we're updating loops
	' correctly....
  if buffer_mode = 1
    call update_music_vbi
  else
    call play_that_music
  endif

  controls = WaitForFrame(JoystickNone, Controller1, JoystickNone)
endwhile


' this function is only used in buffer mode :)
' the algorythm is:
' 0) update ayc_buffer_played from what actually got played last frame... this needs to be done first so we
'    know where we are!
' 1) get where we SHOULD be from the vectrex32 tick counter, and convert that into frames (in music_target)
' 2) compare that to where we are (in ayc_buffer_played), which is already in frames
' 3) set wait_ time to the difference between these, which since it's constantly playing, SHOULD be only the 
'    fractional part
' 4) shove that into the VIA countdown register 2 that's normally used for vx refresh
sub update_music_vbi
  ayc_played_this_frame = 0
  if ayc_buffer_played >= 0 
		' wait for the dualport to have returned
		' why is this a sequence?  because if not, bad things happen in the bathroom...
		' once we've hit it, we update the codesprite to chang the sequence for the next run, so that we
		' don't lose track	
		while Peek(dualport_status) != ayc_dp_sequence
		endwhile
		ayc_dp_sequence = (ayc_dp_sequence + 4) mod 256
		ayc_exit[2] = ayc_dp_sequence
	  ' fill any used buffers with new sound data
  	ayc_played_this_frame = Peek(dualport_return)
    for i = 1 to ayc_played_this_frame
      call play_that_music
    next
    ayc_buffer_played = ayc_buffer_played + ayc_played_this_frame
  	'print "Played "+ayc_played_this_frame+" full "+ayc_buffer_played
  else
    ayc_buffer_played = 0
  endif


	if buffer_mode_preserve_refresh = 1
		target_tick = (game_frame_count * 960) / GetFrameRate()
  	while (GetTickCount() - ayc_start_time) < target_tick
		endwhile
		game_frame_count = game_frame_count + 1
	endif


  ' This is all terrible and absolutely should
  ' NOT be fpmath, which is almost certainly slow as hell.  But it also might not be
  ' worht optimizing.... 
  current_tick = GetTickCount() - ayc_start_time 
  ' where should be for the _next_ frame
  music_target = (current_tick / 960.0) * player_rate + 1
  ' ... vs where are we right now...
  played_to = ayc_buffer_played


  ' music_target _SHOULD_ be ahead... as a general rule - the player is _triggered_ on the x.00 tick,
  ' and so music target should, as a general rule, be above that - and we're waiting for the next whole
  ' number to tick over....
  wait_time =  played_to - music_target

  ' wait_time gets multiplied by via_wait - it should be fractional, so this should work out....
  wait_time = wait_time * via_rate
  wait_time = Int(wait_time)
  if wait_time < 2
    wait_time = 2
  endif
  if wait_time > 65535
    wait_time = 65535
  endif
  print "AYC: (last: "+ayc_played_this_frame+") music target is "+music_target+" for tick "+current_tick, " vs " + played_to + " wait_time: "+wait_time

  ' shove that wait_time in the codesptie for the VIA, so we wait for that
  ayc_init[2] = wait_time mod 256
  ayc_init[3] = wait_time / 256
  
endsub

' generate a codesprite with lda #imm, sta buffer_base+offset
sub generate_ayc_pokedata_codesprite()
  addr = buffer_base
  offset = 1
  for b = 1 to buffer_count
    for r = 1 to max_regs
        ' incr addr to skip channel set
        addr = addr + 1
        ' lda_imm
        ayc_pokedata[offset] = $86
        offset = offset + 1
        ' this will be filled in later!
        ayc_pokedata[offset] = (r-1)
        offset = offset + 1
        ' sta_abs, hi, lo
        ayc_pokedata[offset] = $b7
        offset = offset + 1
        ayc_pokedata[offset] = (addr / 256) mod 256
        offset = offset + 1
        ayc_pokedata[offset] = addr mod 256
        offset = offset + 1
        ' incr addr
        addr = addr + 1
    next
    ' skip the $ff
    addr = addr + 1
  next
endsub

sub fill_buffer(outregs)
  for r = 1 to 14
    ' 5 bytes per "reg"
    ' our write is at +6 
    ' + 29*5*current_buffer -> buf ptr (the other part is the $ff) (2 x per reg + $ff)
    ' +1 for gsbasic
    'print "r:"+r+"  addr:"+((((r-1)*5)+1)+(14*5*current_buffer)+1)
    ayc_pokedata[(((r-1)*5)+1)+(14*5*current_buffer)+1] = outregs[r,2]
  next
  current_buffer = (current_buffer + 1) mod buffer_count
endsub


sub load_and_init(filename)
  if buffer_mode = 1
    dim pd[2]
    call clearscreen()
    call TextSprite("AYC Loader")
    call pokeRAM(buffer_location, (buffer_base / 256) mod 256)
    call pokeRAM(buffer_location+1, buffer_base mod 256)
    addr = buffer_base
    for b = 1 to buffer_count
      for reg = 1 to 14
        for v = 1 to 2
          call pokeRAM(addr, reg-1)
          addr = addr + 1
        next
      next
      call pokeRAM(addr, $ff)
      addr = addr + 1
    next
    ' why do i need two of these?  If I have one, it doens't seem to work at all.....
    ' I have no idea what i'm doing wrong :)
    controls = WaitForFrame(JoystickNone, Controller2, JoystickNone)
    'call Peek(buffer_location, 2, pd)
    controls = WaitForFrame(JoystickNone, Controller2, JoystickNone)
    call clearscreen()
    'while pd[1] = 0
    'endwhile
    'data = pd[2]
    'print data
  endif
  ' load the AYC file
  fh = fopen(filename, "rb")
  played_frames = 0
  ayc_duration = fgetc(fh) + fgetc(fh)*256
  header_offset = 0
  print "AYC: Duration ",ayc_duration," frames"
  for reg = 1 to 14
    ay_buffer_sizes[reg] = fgetc(fh)
    if ay_buffer_sizes[reg] = 1
      comp_buffer[reg] = ByteArray(256)
    else
      comp_buffer[reg] = ByteArray(1024)
    endif
    ay_buffer_offsets[reg] = fgetc(fh) + fgetc(fh)*256 
    ' correct for relative offset
    ay_buffer_offsets[reg] = ay_buffer_offsets[reg]+(reg-1)*3+4
    if reg = 1
      header_offset = ay_buffer_offsets[reg] 
    endif
    ' gsbasic fix
    ay_buffer_offsets[reg] = ay_buffer_offsets[reg] - header_offset
    print "AYC: Reg ",reg," buffer size ",ay_buffer_sizes[reg]," @ ",ay_buffer_offsets[reg]
  next

  ' init our flags
  call restart_music

  boffset = 1
  fl = 0
  print "AYC: Skipping from ",ftell(fh)," to ",ay_buffer_offsets[1]+header_offset
  while ftell(fh) != (ay_buffer_offsets[1]+header_offset)
    call fgetc(fh)
  endwhile

  ' read the remainign bytes into the file - we otherwise have buffer pointers...
  i = ftell(fh)
  ay_buffer_data = fread(100000, fh)
  ay_data_length = ftell(fh) - i

  ' fix the buffer pointers?
  for reg = 1 to 14
    ' now read the buffer data - until next buffer offset or EOF
    ' this fixes later the gsbasic's dim is alays start at 1 thing
    ay_buffer_offsets[reg] = ay_buffer_offsets[reg] + 1
  next

  print "AYC: Read ",ftell(fh),"bytes sucessfully"
  
  ' fill our initial buffers and generate our codesprite
  if buffer_mode = 1
    ' generate the data loader
    call generate_ayc_pokedata_codesprite()
    call CodeSprite(ayc_pokedata)
    for i = 1 to buffer_count
      call play_that_music
    next
  endif
endsub

sub restart_reg(reg)
    last_flags[reg] = 0
    comp_flags[reg] = 0
    poffset[reg] = 0
    coffset[reg] = 0
    cursor[reg] = 0
    premaining[reg] = 0
endsub

sub restart_music
  print "AYC: Restart"
  played_frames = 0
    ' init our flags here too
  for reg = 1 to max_regs
    call restart_reg(reg)
  next
endsub

function read_with_coffset(reg)
  if (ay_buffer_offsets[reg] + coffset[reg]) > ay_data_length
    return 0
  endif
  if reg < max_regs and (ay_buffer_offsets[reg] + coffset[reg]) >= ay_buffer_offsets[reg+1]
    return 0
  endif
  return ay_buffer_data[ay_buffer_offsets[reg] + coffset[reg]] 
endfunction

sub play_that_music
  dim outregs[max_regs, 2]
  played_frames = played_frames + 1
  ' reset the data if we're at 0
  if played_frames >= ayc_duration
    call restart_music
  endif

  ' play loop
  for reg = 1 to max_regs
    ' if we're not in a pattern, process next byte
    if premaining[reg] = 0
      ' if the high bit is not set, this is direct data - otherwise it is
      ' a sequence pointer
      if (comp_flags[reg] & 128) = 0
        ' we're direct data - shove it in the AY!
        'print reg," reading from ",ay_buffer_offsets[reg] + coffset[reg]
        my_data = read_with_coffset(reg)
        coffset[reg] = coffset[reg] + 1
        outregs[reg, 1] = reg - 1
        outregs[reg, 2] = my_data
      
        ' shove it in the buffer so we can get it back...
        comp_buffer[reg][cursor[reg]+1] = my_data
        cursor[reg] = cursor[reg] + 1
        cursor[reg] = cursor[reg] mod (ay_buffer_sizes[reg]*256)

      else
        ' 8 bits for length - but multiplied by ay_buffer_sizes[reg]
        ' this is negated - but why?
        premaining[reg] = 256-read_with_coffset(reg)
        coffset[reg] = coffset[reg] + 1

        ' depending on size, 8 or 16bits for offset
        poffset[reg] = read_with_coffset(reg)
        coffset[reg] = coffset[reg] + 1
        if ay_buffer_sizes[reg] != 1
          poffset[reg] = poffset[reg] + read_with_coffset(reg) * 256
          coffset[reg] = coffset[reg] + 1
        endif
        if poffset[reg] > 256*ay_buffer_sizes[reg]
          print "FAIL: ",reg," tries to set poffset of ",poffset[reg], " coffset=",coffset[reg]
          while true
            a=1
          endwhile
        endif
        ' wrap 0
        if premaining[reg] = 0
          premaining[reg] = 256
        endif
      endif
      ' do we have a bitshift in gsbasic?  use it here!
      ' do this before the re-read, so the top bit is always
      ' our relevant one
      comp_flags[reg] = (comp_flags[reg] * 2) & 255
      ' every 8 frames, we grab new flags
      if last_flags[reg] = 0
        comp_flags[reg] = read_with_coffset(reg)
        coffset[reg] = coffset[reg] + 1
        'print reg," -> read flags ",comp_flags[reg]
      endif
      ' This should be an 'and 7', but i had weirdness with that!
      last_flags[reg] = (last_flags[reg] + 1) & 7
    ' process a pattern -
    ' as far as I can tell from the ayc code, patterns can NOT be nested, which 
    ' makes sense since this is originally for z80, and you don't want to risk
    ' blowing the stack
    endif

    ' why is this not an else to the above?  because if it was literally just set, it still needs to run this frame!
    if premaining[reg] > 0
      'print premaining[reg]," -> ",poffset[reg]
      ' we use poffset here, and repeat it until we reach the end
      
      ' this comes from the buffer, according ot the java code...
      'my_data = ay_buffer_data[ay_buffer_offsets[reg] + poffset[reg]]
      'print "reg -> ",reg," -> ",poffset[reg]+1
      my_data = comp_buffer[reg][poffset[reg]+1]
      
      ' shove it in the buffer so we can get it back
      comp_buffer[reg][cursor[reg]+1] = my_data
      cursor[reg] = cursor[reg]+1
      cursor[reg] = cursor[reg] mod  (ay_buffer_sizes[reg]*256) 

      poffset[reg] = poffset[reg] + 1
      poffset[reg] = poffset[reg] mod  (ay_buffer_sizes[reg]*256) 
      premaining[reg] = premaining[reg] - 1
      outregs[reg, 1] = reg - 1
      outregs[reg, 2] = my_data
    endif
  next
  ' these should only ever be 0:16
  if reg = 2 or reg = 4 or reg = 6 
    if outregs[reg][2] > 16
      print "FAIL: reg ",reg," has value ",outregs[reg], " which is too big for its datatype"
       while 0 = 0
          a = 1
       endwhile
    endif
  endif
  ' these 0:32
  if reg = 9 or reg = 10 or reg = 11
    if outregs[reg][2] > 32
      print "FAIL: reg ",reg," has value ",outregs[reg], " which is too big for its datatype"
       while 0 = 0
        a = 1
       endwhile
    endif
  endif
  'print outregs
  if buffer_mode = 0
    call Sound(outregs)
  else
    call fill_buffer(outregs)
  endif
endsub

' Stole this function from Malban's lightpen test - thanks!
sub pokeRAM(where, what)
   if what<0 then
    what = 256 +what
   endif

   poke_RAM = {$86, what, $b7, (where/256) MOD 256, where MOD 256}
   call CodeSprite(poke_RAM)
endsub


' -------------------------------------------------------------------------------
' A test vector
' -------------------------------------------------------------------------------
' final acceptable error: 0.0
' final command count: 371
sub  rose()
  call LinesSprite({ _
    { MoveTo , 14.6060902982 , -16.8818738276 }, _
    { DrawTo , -22.6507938851 , -36.0521700713 }, _
    { DrawTo , 5.81130251164 , -38.3830260146 }, _
    { DrawTo , 64.6983985049 , -4.3949352129 }, _
    { MoveTo , 65.6798501047 , -3.65872044875 }, _
    { DrawTo , 75.4943661036 , -3.90093510615 }, _
    { DrawTo , 87.1382833739 , -11.8076212264 }, _
    { DrawTo , 87.2148223008 , -12.8648011869 }, _
    { DrawTo , 79.4201725032 , -33.8435257788 }, _
    { MoveTo , 60.6018195269 , -41.2056734203 }, _
    { DrawTo , 55.7583558815 , -47.5805570631 }, _
    { DrawTo , 36.6741747743 , -35.9707768349 }, _
    { DrawTo , 34.2733989084 , -33.8435257788 }, _
    { MoveTo , 35.6444867934 , -50.5136366835 }, _
    { DrawTo , 38.2213867861 , -64.5604050919 }, _
    { DrawTo , 38.2374819203 , -66.2369754014 }, _
    { DrawTo , 39.1659351338 , -95.6855659673 }, _
    { DrawTo , 38.1992053079 , -110.40986125 }, _
    { DrawTo , 64.982756703 , -81.9946832223 }, _
    { DrawTo , 64.6100678609 , -75.8077673353 }, _
    { DrawTo , 60.9698638769 , -62.2636243193 }, _
    { DrawTo , 65.4520494264 , -60.9844631151 }, _
    { DrawTo , 69.6056565043 , -60.9583155424 }, _
    { DrawTo , 89.234688502 , -57.1646008628 }, _
    { DrawTo , 124.528669486 , -37.3994430897 }, _
    { DrawTo , 141.251623296 , -25.7451633732 }, _
    { MoveTo , 155.158792466 , -22.8003043166 }, _
    { DrawTo , 150.084687695 , -58.1386129957 }, _
    { DrawTo , 130.458728042 , -58.2871438734 }, _
    { DrawTo , 128.586812861 , -57.4506200392 }, _
    { DrawTo , 128.274870242 , -57.1675457218 }, _
    { MoveTo , 126.529849298 , -56.6661834674 }, _
    { DrawTo , 124.566946098 , -58.1386129957 }, _
    { DrawTo , 47.0322697069 , -127.774222677 }, _
    { DrawTo , 10.7185605111 , -149.42924375 }, _
    { DrawTo , 17.8228193473 , -135.634024859 }, _
    { DrawTo , 20.3799700604 , -133.231782724 }, _
    { DrawTo , 37.217753708 , -110.40986125 }, _
    { MoveTo , 11.700012111 , -104.520143137 }, _
    { DrawTo , 11.700012111 , -137.649807524 }, _
    { MoveTo , 9.73710891119 , -149.42924375 }, _
    { DrawTo , -9.01352390465 , -203.909136297 }, _
    { DrawTo , 11.700012111 , -223.050720165 }, _
    { MoveTo , 6.79275411152 , -222.314505401 }, _
    { DrawTo , -19.7064390854 , -226.731793986 }, _
    { MoveTo , -29.1902058951 , -219.36891013 }, _
    { DrawTo , -9.76040857217 , -238.511230212 }, _
    { DrawTo , 0.269045327077 , -248.81823691 }, _
    { DrawTo , -5.40735020897 , -256.857558208 }, _
    { DrawTo , -21.6693422852 , -263.966591897 }, _
    { DrawTo , -28.0566292973 , -271.612182223 }, _
    { DrawTo , 6.17890392189 , -280.98411756 }, _
    { DrawTo , 24.4588829095 , -280.475471769 }, _
    { DrawTo , 155.973397294 , -280.475471769 }, _
    { DrawTo , 207.008880488 , -278.266827476 }, _
    { MoveTo , 207.455440966 , -278.046699262 }, _
    { DrawTo , 234.489525285 , -294.463552287 }, _
    { DrawTo , 252.717044398 , -310.660277099 }, _
    { DrawTo , 256.0 , -317.702907533 }, _
    { DrawTo , 248.881134762 , -319.887884365 }, _
    { DrawTo , 242.089433059 , -319.967117641 }, _
    { DrawTo , 231.545170486 , -319.494854268 }, _
    { DrawTo , 158.917752094 , -319.494854268 }, _
    { DrawTo , -146.313695471 , -319.494854268 } _
  })
  call ReturnToOriginSprite()
  call LinesSprite({ _
    {MoveTo, -146.313695471 , -319.494854268 }, _
    { DrawTo , -229.737081461 , -319.494854268 }, _
    { DrawTo , -251.341229709 , -319.632408328 }, _
    { DrawTo , -256.0 , -317.702907533 }, _
    { DrawTo , -254.127123492 , -311.336049548 }, _
    { DrawTo , -239.135069401 , -298.144626108 }, _
    { DrawTo , -207.049355552 , -278.382413194 }, _
    { DrawTo , -178.56527464 , -245.873377854 }, _
    { DrawTo , -169.864706207 , -178.877834316 }, _
    { DrawTo , -169.720138386 , -88.3234183258 }, _
    { DrawTo , -167.905630668 , -36.7883848354 }, _
    { MoveTo , -166.924179068 , -36.0521700713 }, _
    { DrawTo , -144.350792271 , -20.9811176344 }, _
    { DrawTo , -132.232809367 , -14.6592414547 }, _
    { DrawTo , -118.0518152 , 21.3431329417 }, _
    { DrawTo , -92.8952477922 , 47.1400982775 }, _
    { DrawTo , -82.0384301942 , 54.8666722272 }, _
    { DrawTo , -76.6306318789 , 65.5454673812 }, _
    { DrawTo , -62.8903094804 , 60.6592099916 }, _
    { DrawTo , -53.0757934816 , 55.7994563334 }, _
    { DrawTo , -36.3911162835 , 58.8981842757 }, _
    { DrawTo , -21.6654164788 , 55.9216679843 }, _
    { DrawTo , -14.5020118658 , 60.6656021427 }, _
    { DrawTo , -14.7037010097 , 60.9627620814 }, _
    { DrawTo , -15.2820552731 , 61.5794784467 }, _
    { MoveTo , -12.8362778862 , 58.9195345039 }, _
    { DrawTo , -4.98466508712 , 59.655749268 }, _
    { MoveTo , -3.24062559412 , 61.447696004 }, _
    { DrawTo , 3.84839931186 , 59.4164794697 }, _
    { DrawTo , 20.5330765099 , 53.7660311548 }, _
    { MoveTo , 25.9251715997 , 79.5335479 }, _
    { DrawTo , 6.30693556958 , 30.4272869166 }, _
    { DrawTo , -16.7620842858 , 19.2655348773 }, _
    { DrawTo , -33.4467614838 , 11.4027611962 }, _
    { DrawTo , -34.1771174996 , 11.332786789 }, _
    { DrawTo , -58.9645030809 , 23.8366923479 }, _
    { MoveTo , -61.6782039391 , 24.1146432372 }, _
    { DrawTo , -62.52760416 , 24.1013843778 }, _
    { DrawTo , -81.6262205223 , 17.0421662896 }, _
    { DrawTo , -82.3475194177 , 17.0007837593 }, _
    { DrawTo , -109.999986275 , 25.053655353 }, _
    { MoveTo , -78.5935350786 , 15.4828634191 }, _
    { DrawTo , -61.1344925682 , 1.49478290029 }, _
    { DrawTo , -39.3325267284 , -23.5365190808 }, _
    { DrawTo , -22.6507938851 , -36.0521700713 }, _
    { MoveTo , -27.5580518845 , -36.0521700713 }, _
    { DrawTo , -41.3975008945 , -59.611042524 }, _
    { DrawTo , -30.1098260442 , -71.9382225349 }, _
    { DrawTo , -28.274986646 , -68.8314633498 }, _
    { DrawTo , -28.1086462321 , -61.0834720523 }, _
    { DrawTo , -12.8362778862 , -40.4694586562 }, _
    { MoveTo , -5.966116687 , -41.2056734203 }, _
    { DrawTo , 23.4774313096 , -49.503550027 }, _
    { DrawTo , 35.6444867934 , -50.5136366835 }, _
    { MoveTo , 47.0322697069 , -44.8867472411 }, _
    { DrawTo , 37.217753708 , -50.0402505901 }, _
    { MoveTo , 12.6814637108 , -49.304035826 }, _
    { DrawTo , 14.0525515959 , -80.1853003229 }, _
    { DrawTo , -6.25270055417 , -92.3689184548 }, _
    { DrawTo , -5.48276915232 , -98.773740147 }, _
    { DrawTo , 2.86694771198 , -112.618505543 }, _
    { DrawTo , 13.6629153107 , -82.4337002126 }, _
    { MoveTo , 1.88549611209 , -113.354720307 }, _
    { DrawTo , -19.7064390854 , -145.748169929 }, _
    { MoveTo , -22.4662809843 , -148.693028986 } _
  })
  call CodeSprite(ayc_playcode)
  call ReturnToOriginSprite()
  call LinesSprite({ _
    {MoveTo, -22.4662809843 , -148.693028986 }, _
    { DrawTo , -35.9023533868 , -161.484760513 }, _
    { DrawTo , -42.2798258828 , -170.043257146 }, _
    { MoveTo , -45.2241806825 , -178.141619552 }, _
    { DrawTo , -41.4897573449 , -192.865914835 }, _
    { DrawTo , -29.1902058951 , -219.36891013 }, _
    { MoveTo , -44.9709661697 , -241.456089269 }, _
    { DrawTo , -38.0428993261 , -251.282347726 }, _
    { DrawTo , -0.0774070876832 , -251.026881203 }, _
    { MoveTo , 1.88549611209 , -249.554451674 }, _
    { DrawTo , 57.8282373056 , -238.000297166 }, _
    { DrawTo , 138.307268496 , -203.655878418 }, _
    { DrawTo , 168.732268093 , -186.976196722 }, _
    { MoveTo , 170.691245486 , -146.484384694 }, _
    { DrawTo , 171.971058372 , -230.412867807 }, _
    { DrawTo , 207.455440966 , -278.046699262 }, _
    { MoveTo , 189.34275169 , -287.83761941 }, _
    { DrawTo , 220.749202887 , -286.365189882 }, _
    { MoveTo , 116.715333299 , -287.83761941 }, _
    { DrawTo , 189.34275169 , -287.83761941 }, _
    { MoveTo , 117.430811515 , -133.968733703 }, _
    { MoveTo , 105.591560866 , -119.991696406 }, _
    { MoveTo , 105.591560866 , -119.991696406 }, _
    { DrawTo , 126.29822672 , -103.783928373 }, _
    { DrawTo , 150.084687695 , -58.1386129957 }, _
    { MoveTo , 135.928229818 , -42.6781029486 }, _
    { DrawTo , 153.187056202 , -11.6319263445 }, _
    { DrawTo , 164.806461693 , -0.713861392155 }, _
    { MoveTo , 165.818338293 , 0.0223533719935 }, _
    { DrawTo , 170.691245486 , -146.484384694 }, _
    { MoveTo , 108.8637205 , -66.2369754014 }, _
    { DrawTo , 124.566946098 , -58.1386129957 }, _
    { MoveTo , 127.752422975 , -54.2772091377 }, _
    { DrawTo , 135.928229818 , -42.6781029486 }, _
    { MoveTo , 104.9379141 , -52.2488948826 }, _
    { DrawTo , 108.8637205 , -66.2369754014 }, _
    { MoveTo , 107.8822689 , -66.9731901655 }, _
    { DrawTo , 65.6798501047 , -94.213136439 }, _
    { MoveTo , 60.7725921053 , -122.925512241 }, _
    { DrawTo , 103.9564625 , -119.980653184 }, _
    { MoveTo , 55.8653341059 , -22.0640895525 }, _
    { DrawTo , 60.6018195269 , -41.2056734203 }, _
    { MoveTo , 63.716946905 , -15.4381566751 }, _
    { DrawTo , 79.4201725032 , -30.1624519581 }, _
    { MoveTo , 36.3050037201 , 14.0104338908 }, _
    { DrawTo , 14.6060902982 , -16.8818738276 }, _
    { MoveTo , 4.82985091175 , -47.8316062977 }, _
    { DrawTo , -6.94756828689 , -64.7645458731 }, _
    { MoveTo , -19.7064390854 , -48.5678210618 }, _
    { MoveTo , -43.2612774827 , -83.843551486 }, _
    { DrawTo , -20.4409934251 , -117.780964091 }, _
    { DrawTo , -20.8586632637 , -122.189297477 }, _
    { DrawTo , -25.4557825576 , -136.91359276 }, _
    { DrawTo , -22.4662809843 , -148.693028986 }, _
    { MoveTo , -41.2983742829 , -139.858451816 }, _
    { DrawTo , -37.3725678834 , -160.472465212 }, _
    { MoveTo , -58.3726877662 , -167.12931911 }, _
    { DrawTo , -60.9518473938 , -175.728969459 }, _
    { DrawTo , -58.9645030809 , -189.921055778 }, _
    { DrawTo , -42.2798258828 , -170.043257146 }, _
    { MoveTo , -58.3726877662 , -167.12931911 }, _
    { MoveTo , -58.9645030809 , -189.921055778 }, _
    { DrawTo , -73.7647932072 , -233.357726863 }, _
    { DrawTo , -75.5343504418 , -248.738725716 }, _
    { DrawTo , -65.9229949241 , -253.298839965 } _
  })
  call ReturnToOriginSprite()
  call LinesSprite({ _
    {MoveTo, -65.9229949241 , -253.298839965 }, _
    { DrawTo , -50.1314386819 , -262.062004302 }, _
    { DrawTo , -30.5024066842 , -268.696035542 }, _
    { MoveTo , -19.7064390854 , -278.266827476 }, _
    { DrawTo , -71.7233738794 , -280.475471769 }, _
    { MoveTo , -63.8717610803 , -253.235525495 }, _
    { DrawTo , -41.2983742829 , -251.763095967 }, _
    { MoveTo , -63.8717610803 , -209.062639646 }, _
    { DrawTo , -44.9709661697 , -241.456089269 }, _
    { MoveTo , -118.41985955 , -198.755632948 }, _
    { DrawTo , -172.812888668 , -237.038800684 }, _
    { MoveTo , -205.200791464 , -279.739257004 }, _
    { MoveTo , -219.922565462 , -286.365189882 }, _
    { DrawTo , -190.479017466 , -287.83761941 }, _
    { DrawTo , -124.721760273 , -287.83761941 }, _
    { DrawTo , 116.715333299 , -287.83761941 }, _
    { MoveTo , -40.8253146118 , -129.551445118 }, _
    { DrawTo , -26.5766002846 , -139.858451816 }, _
    { MoveTo , -62.8903094804 , -137.134457189 }, _
    { DrawTo , -35.4096646836 , -139.858451816 }, _
    { MoveTo , -56.1496998924 , -116.299579363 }, _
    { DrawTo , -40.8253146118 , -129.551445118 }, _
    { MoveTo , -52.5634757464 , -103.783928373 }, _
    { DrawTo , -56.1496998924 , -116.299579363 }, _
    { MoveTo , -50.1314386819 , -82.4337002126 }, _
    { DrawTo , -52.5634757464 , -103.783928373 }, _
    { MoveTo , -91.3524058772 , -145.011955165 }, _
    { DrawTo , -62.8903094804 , -137.134457189 }, _
    { MoveTo , -92.2239348979 , -146.484384694 }, _
    { DrawTo , -118.41985955 , -198.755632948 }, _
    { MoveTo , -168.887082268 , -172.988116203 }, _
    { DrawTo , -129.629018273 , -209.79885441 }, _
    { MoveTo , -119.814502274 , -102.377758173 }, _
    { DrawTo , -92.2239348979 , -146.484384694 }, _
    { MoveTo , -141.406437471 , -88.3234183258 }, _
    { DrawTo , -58.9645030809 , -114.827149835 }, _
    { MoveTo , -134.536276272 , -78.0164116278 }, _
    { DrawTo , -43.2612774827 , -83.843551486 }, _
    { MoveTo , -136.920222208 , -54.457539175 }, _
    { DrawTo , -134.536276272 , -78.0164116278 }, _
    { MoveTo , -153.18385667 , -86.8509887975 }, _
    { DrawTo , -119.814502274 , -102.377758173 }, _
    { MoveTo , -168.887082268 , -88.3234183258 }, _
    { DrawTo , -135.517727872 , -78.7526263919 }, _
    { MoveTo , -133.554824672 , -18.3830157317 }, _
    { DrawTo , -136.920222208 , -54.457539175 }, _
    { MoveTo , -165.981396662 , 2.23099766444 }, _
    { DrawTo , -166.924179068 , -36.0521700713 }, _
    { MoveTo , -81.5378898783 , 53.7660311548 }, _
    { DrawTo , -59.9459546808 , 44.9314539851 }, _
    { MoveTo , -56.8131611739 , 46.3405690436 }, _
    { DrawTo , -57.6623171108 , 43.5799505552 }, _
    { DrawTo , -50.8390652854 , 29.189709898 }, _
    { DrawTo , -33.4467614838 , 30.6304821915 }, _
    { DrawTo , 1.88549611209 , 28.7347291738 }, _
    { MoveTo , 5.81130251164 , 28.7347291738 }, _
    { DrawTo , 21.5145281098 , 23.0666117046 }, _
    { DrawTo , 36.2363021081 , 14.746648655 }, _
    { MoveTo , 51.9395277063 , 39.0417358719 }, _
    { DrawTo , 36.3050037201 , 14.0104338908 }, _
    { MoveTo , 51.6725728712 , 40.3021355481 }, _
    { DrawTo , 55.1716279986 , 40.369884803 }, _
    { DrawTo , 74.5129145037 , 46.4038835134 }, _
    { DrawTo , 65.6798501047 , -3.65872044875 }, _
    { MoveTo , 75.4943661036 , 47.1400982775 } _
  })
  call CodeSprite(ayc_playcode)
  call ReturnToOriginSprite()
  call LinesSprite({ _
    {MoveTo, 75.4943661036 , 47.1400982775 }, _
    { DrawTo , 91.1975917018 , 48.6125278058 }, _
    { MoveTo , 93.7523102163 , 49.4775801537 }, _
    { DrawTo , 119.659688099 , 44.1930305766 }, _
    { DrawTo , 160.880655294 , 27.9985144096 }, _
    { MoveTo , 159.771614986 , 50.0849573341 }, _
    { DrawTo , 165.818338293 , 0.0223533719934 }, _
    { MoveTo , 153.029042495 , -12.4932976185 }, _
    { DrawTo , 155.158792466 , -22.8003043166 }, _
    { MoveTo , 91.106316703 , 55.9746754473 }, _
    { DrawTo , 93.7523102163 , 49.4775801537 }, _
    { MoveTo , 82.876845038 , 67.7541116737 }, _
    { DrawTo , 91.106316703 , 55.9746754473 }, _
    { MoveTo , 63.6708186798 , 66.2816821454 }, _
    { DrawTo , 51.6725728712 , 40.3021355481 }, _
    { MoveTo , 57.8282373056 , 79.5335479 }, _
    { DrawTo , 70.5483521813 , 77.2885388834 }, _
    { DrawTo , 82.876845038 , 67.7541116737 }, _
    { MoveTo , 56.588663935 , 79.9509816713 }, _
    { DrawTo , 63.6708186798 , 66.2816821454 }, _
    { MoveTo , 48.0137213068 , 81.0059774283 }, _
    { DrawTo , 23.4774313096 , 50.8211720982 }, _
    { MoveTo , 1.88549611209 , 75.1162593151 }, _
    { DrawTo , 21.6660939397 , 75.3897619015 }, _
    { DrawTo , 24.4588829095 , 74.380044551 }, _
    { MoveTo , 0.904044512204 , 74.380044551 }, _
    { DrawTo , -3.24062559412 , 61.447696004 }, _
    { MoveTo , -25.5951486847 , 55.9746754473 }, _
    { DrawTo , -56.8131611739 , 46.3405690436 }, _
    { MoveTo , -53.0757934816 , 29.4709439379 }, _
    { DrawTo , -58.9645030809 , 24.3174405889 }, _
    { MoveTo , -46.315643746 , 66.1031350521 }, _
    { DrawTo , -52.0943418817 , 66.5872112725 }, _
    { MoveTo , -57.0015998811 , 68.4903264378 }, _
    { DrawTo , -38.3540194833 , 89.104339834 }, _
    { MoveTo , -36.3636356387 , 90.5767693623 }, _
    { DrawTo , -29.7643550811 , 78.0611183717 }, _
    { DrawTo , -35.4096646836 , 67.7541116737 }, _
    { MoveTo , -27.5580518845 , 78.7973331359 }, _
    { DrawTo , -7.24494812166 , 87.5789028427 }, _
    { DrawTo , 1.88549611209 , 75.1162593151 }, _
    { MoveTo , 8.7556573113 , 104.564849881 }, _
    { DrawTo , 36.0697482134 , 101.364322572 }, _
    { DrawTo , 41.1435601076 , 100.147561296 }, _
    { MoveTo , 42.0788834823 , 100.147561296 }, _
    { DrawTo , 49.0226535515 , 83.4457931567 }, _
    { DrawTo , 56.588663935 , 79.9509816713 }, _
    { MoveTo , 46.8948664829 , 124.442648513 }, _
    { DrawTo , 42.0788834823 , 100.147561296 }, _
    { MoveTo , 50.9580761064 , 134.013440447 }, _
    { DrawTo , 88.2532369021 , 122.675953944 }, _
    { DrawTo , 159.771614986 , 50.0849573341 }, _
    { MoveTo , 49.1001882278 , 134.544545778 }, _
    { DrawTo , 48.2353808872 , 127.224396936 }, _
    { DrawTo , 46.8948664829 , 124.442648513 }, _
    { MoveTo , 37.217753708 , 138.430729032 }, _
    { DrawTo , 49.1001882278 , 134.544545778 }, _
    { MoveTo , 27.4032377091 , 135.794859312 }, _
    { DrawTo , 23.906425975 , 135.690109387 }, _
    { DrawTo , 13.6629153107 , 138.188146267 }, _
    { DrawTo , 2.86694771198 , 134.182180871 }, _
    { DrawTo , 0.278319387452 , 134.065053414 }, _
    { DrawTo , -12.9246085302 , 138.306897709 }, _
    { DrawTo , -13.5228981442 , 138.32971618 }, _
    { DrawTo , -21.6693422852 , 135.802810431 } _
  })
  call ReturnToOriginSprite()
  call LinesSprite({ _
    {MoveTo, -21.6693422852 , 135.802810431 }, _
    { DrawTo , -26.5487202858 , 135.700918886 }, _
    { DrawTo , -35.4096646836 , 139.166943796 }, _
    { MoveTo , -38.587604964 , 138.496031281 }, _
    { DrawTo , -62.8903094804 , 132.227972401 }, _
    { DrawTo , -100.185470276 , 116.765400953 }, _
    { DrawTo , -165.981396662 , 2.23099766444 }, _
    { MoveTo , -135.517727872 , 84.6870512491 }, _
    { DrawTo , -76.6306318789 , 68.4903264378 }, _
    { MoveTo , -71.704726299 , 70.8778709179 }, _
    { DrawTo , -76.0696593997 , 68.1198074217 }, _
    { DrawTo , -76.5756026576 , 66.029148891 }, _
    { DrawTo , -76.6306318789 , 65.5454673812 }, _
    { MoveTo , -71.704726299 , 70.8778709179 }, _
    { MoveTo , -71.7233738794 , 71.4351854944 }, _
    { DrawTo , -55.7681563414 , 84.4672200066 }, _
    { DrawTo , -54.8797687291 , 84.5752099682 }, _
    { DrawTo , -53.0757934816 , 84.6870512491 }, _
    { MoveTo , -21.6693422852 , 106.773494174 }, _
    { DrawTo , -36.3636356387 , 90.5767693623 }, _
    { MoveTo , -19.1833253827 , 110.423646974 }, _
    { DrawTo , -2.01086673946 , 114.637740284 }, _
    { DrawTo , 3.17708641754 , 106.503303355 }, _
    { DrawTo , 25.9251715997 , 79.5335479 }, _
    { MoveTo , 0.904044512204 , 106.037279409 }, _
    { DrawTo , -15.59476181 , 105.932621208 }, _
    { DrawTo , -20.2020358055 , 107.852075814 }, _
    { DrawTo , -20.2926157398 , 108.268430359 }, _
    { DrawTo , -19.1833253827 , 110.423646974 }, _
    { MoveTo , -12.8362778862 , 148.001520966 }, _
    { DrawTo , -12.8362778862 , 139.166943796 }, _
    { MoveTo , -4.27104886479 , 152.155810511 }, _
    { DrawTo , -5.10940277226 , 151.928517504 }, _
    { DrawTo , -12.8362778862 , 148.001520966 }, _
    { MoveTo , -25.5951486847 , 153.891239079 }, _
    { DrawTo , -35.2644098468 , 146.528502466 }, _
    { DrawTo , -38.587604964 , 138.496031281 }, _
    { MoveTo , -27.8809494609 , 158.308527664 }, _
    { MoveTo , -32.411330046 , 170.824178654 }, _
    { MoveTo , -32.411330046 , 170.824178654 }, _
    { DrawTo , -13.1418850336 , 190.859125576 }, _
    { DrawTo , -7.17649952394 , 192.032882359 }, _
    { DrawTo , 28.1181175203 , 181.572441337 }, _
    { DrawTo , 29.6136843768 , 179.487913618 }, _
    { DrawTo , 32.2957739346 , 169.351749126 }, _
    { DrawTo , 28.497556243 , 158.309116636 }, _
    { DrawTo , 13.6629153107 , 148.001520966 }, _
    { DrawTo , 13.6629153107 , 139.166943796 }, _
    { MoveTo , 27.4032377091 , 152.418809551 }, _
    { DrawTo , 35.6118984559 , 140.801880791 }, _
    { DrawTo , 35.3112957529 , 140.196878416 }, _
    { DrawTo , 27.4032377091 , 135.794859312 } _
  })
endsub
