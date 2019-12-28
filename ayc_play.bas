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
' Is the vectrex32 responsible fopr timing, or the music routine?  
' 
' Note: If you do this, you are required to time your _entire_ game from vectrex32 tick counter, since it WILL override your
' chosen refresh rate.  
'
buffer_mode_preserve_refresh = 1
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
  ' number of buffers
  buffer_count = 1
  ' rate of playback - 50hz by default...
  player_rate = 50
  ' This is almost certainly wrong/destructive!
  ' but is.... probably enough?
  dualport_return = 0
  dualport_status = 1
  ' you'll need to allow for max_regs*buffer_count worth of iram at this location 
  ' if this is the only weird thing you're using, c882 should be fine.  c880 is better, but doens't work
  ' on all v32 firmware revisions right now....
  buffer_location = $c882
  buffer_base = $c884

  ' below here is not, for the most part, user servicable :)
  via_rate = 1500000 / player_rate 
  tick_rate = 960 / player_rate
  print "AYC: VIA Rate is "+via_rate+" cycles"
  print "AYC: Tick Rate is "+tick_rate+" cycles"
  current_buffer = 0
  ' we use this being negative to represent first frame
  ayc_buffer_played = -1
  dim ayc_pokedata[max_regs*5*buffer_count]
  dim ay_output_data[buffer_count, max_regs*2]
  '--------------------------------------------------------------------'
  ' should be in hex format, but for now this is what we get!
  ' the listing for this is an other file within the github, but I built it with 
  ' asm80.com :)
  '--------------------------------------------------------------------'
  ' first line: ldd $d008/bne no_play - check via
  ' second line: call sound_bytes_x, increment dualport return
     '$fc,$d0,$08,$26,$09, _
  ' third line: write to VIA for next countdown timer
  ' fourth line: incremener buffer
  ayc_playcode = { _
     $FE, buffer_location / 256, buffer_location mod 256, $BD, $F2, $7D, $7c, dualport_return / 256, dualport_return mod 256 }

  print ayc_playcode

  '--------------------------------------------------------------------'
  ' this sets up the timer we need to keep time on the VX side...
  ' this will be modified by the player to set it to what's remaining for the first
  ' vblank that we need.  it should be called as early as posisble during your dualport config
  '
  ' It also resets the dualport_return register to 0 - that register ends up containing how many frames were acutally played!
	' we also store $ff in dualport_status, so that we know that we're currently running - we reset this at ayc_exit, to ensure that
	' we only call the AY update code when the vectrex is idle, otherwise buffers get _very_ confused.
  '--------------------------------------------------------------------'
	' 0000   CC 30 75               LDD   #$3075  ; this gets replaced by wait_time for first music call  
	' 0003   FD D0 08               STD   $d008   
	' 0006   86 00                  LDA   #0   
	' 0008   B7 01 23               STA   $123  ; this gets replaced with dualport_return
  ayc_init = { $cc, $30, $75, $fd, $d0, $08, $86, $00, $b7, dualport_return / 256, dualport_return mod 256, _
			$86, $ff, $b7, dualport_status / 256, dualport_status mod 256 }

  '--------------------------------------------------------------------
  ' this resets the VIA at the end, so that wait_recal doens't wait - this should be the last thing you call.
	' note that, in VIA buffered mode, all it does is update dualport_status to $fe
  '--------------------------------------------------------------------
	if buffer_mode_preserve_refresh = 0
	  ayc_exit = { $86, $fe, $b7, dualport_status / 256, dualport_status mod 256, _
								 $cc, $1, $0, $fd, $d0, $08}
	else
	  ayc_exit = { $86, $fe, $b7, dualport_status / 256, dualport_status mod 256 }
	endif

endif

' set the framerate to 50fps, since that is what most AYC tracks are
' in buffer mode, we set it just as fast as we can update it - our buffer code
' will actually cause this to be ignored anyhow....
if buffer_mode = 1
	' 150 seems to be the max framerate the vectrex32 wil actually set
	' if you don't set it, it will forcably overwrite our VIA settings anyhow, so we set this
	' as high as we can, and then override it in our codesprites....
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
call TextSprite("AYC TEST")
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
	' safety: wait until we do stuff, since we have _no other code_.  you probably don't
	' need this in teh real world...
  if buffer_mode = 1
		while Peek(dualport_status) != 255
		endwhile
	endif
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
  if ayc_buffer_played >= 0 
		' wait for the dualport to have returned
		while Peek(dualport_status) != 254
		endwhile
	  ' fill any used buffers with new sound data
  	ayc_played_this_frame = Peek(dualport_return)
		print ayc_played_this_frame
    for i = 1 to ayc_played_this_frame
      call play_that_music
    next
    ayc_buffer_played = ayc_buffer_played + ayc_played_this_frame
  	print "Played "+ayc_played_this_frame+" full "+ayc_buffer_played
  else
    ayc_buffer_played = 0
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
  print "music target is "+music_target+" for tick "+current_tick, " vs " + played_to + " wait_time: "+wait_time

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
  'print "fill:",current_buffer
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
