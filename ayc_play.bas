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

' should we use IRQ based timing, or Poke based timing?
irq_mode = 0

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
  ' This is almost certainly wrong/destructive!
  ' but is.... probably enough?
  dualport_return = 1
  dualport_status = 2
  dualport_flag = 3
  ayc_buffer_overflow = false
  '--------------------------------------------------------------------'
  ' This is only required if buffer_mode is set to 1 - you can save a few byres of ram by excluding it if you want, otherwise
  ' number of buffers.  
  '
  ' Currently, we non-optionally consume 70 bytes of dpram per buffer - so 4 buffers would be 280 bytes of dpram. 
  lframe = 0
  buffer_count = 6

  ' rate of playback - 50hz by default...
  player_rate = 50
  ' you'll need to allow for max_regs*buffer_count worth of iram at this location 
  ' if this is the only weird thing you're using, c882 should be fine.  c880 is better, but doens't work
  ' on all v32 firmware revisions right now....
  buffer_location = $c882
  buffer_base = $c884
  ' buffers are 14*2 + 1 bytes long
  buffer_end = buffer_base + (buffer_count - 1) * 29
  ' add an extra buffer_end for this, because of that way it's calculated...
  if irq_mode = 1
    flag_loc = buffer_end + 29
  else
    flag_loc = dualport_flag
  endif
  ayc_ticked = 0
  player_code_loc = buffer_end + 31
  player_jmp = player_code_loc + 10

  print "player_jmp: "+player_jmp+" player_code_loc: "+player_code_loc

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
  ' there are two bits of code here:
  ' a) the irq hander - this just sets a flag, to avoid messing up line drawing
  ' b) the actual player code itself - this is called outside, so that we know we're not drawing at the time
  '
  ' lines 1-3 are the irq handler
  ' rest is plaer
  '
  ' first line: check if we play ;)
  ' then next overflow check
  ' thjen  call sound_bytes_x, increment dualport return
  ' then incremener buffer pointer
  ' then finally an RTS ;)
  '
  ' orig code is in buffer.a09 with comments
  '
  ' we acutally shove the main playcode into vectrex ram to try and save ourselves some dpram....
  ' see the .lst file for disas
  ayc_playcode = { $bd, player_jmp / 256, player_jmp mod 256 }
  internal_ayc_playcode = { _
     $cc, via_rate mod 256, via_rate / 256, $fd, $d0, $08, _ 
     $7c, flag_loc / 256, flag_loc mod 256, _
     $3b, _
     $b6, flag_loc / 256, flag_loc mod 256, _
     $81, $00, _
     $27, $2a, _
     $4a, _
     $b7, flag_loc / 256, flag_loc mod 256, _
     $b6, dualport_return / 256, dualport_return mod 256, _
     $81, buffer_count, _
     $2c, $1f, _
     $FE, buffer_location / 256, buffer_location mod 256, _
     $BD, $F2, $7D, _
     $b6, dualport_return/256, dualport_return mod 256, _
     $4c, _
     $b7, dualport_return/256, dualport_return mod 256, _
     $fc, buffer_location / 256, buffer_location mod 256, _
     $c3, $00, $1d, _
     $10, $83, buffer_end / 256, buffer_end mod 256,  _
     $2f, $03, _
     $cc, buffer_base / 256, buffer_base mod 256, _
     $fd, buffer_location / 256, buffer_location mod 256, _
     $39 }

  '--------------------------------------------------------------------'
  ' this sets up the timer we need to keep time on the VX side...
  ' this will be modified by the player to set it to what's remaining for the first
  ' vblank that we need.  it should be called as early as posisble during your dualport config
  '
  ' It also resets the dualport_return register to 0 - that register ends up containing how many frames were acutally played!
  '--------------------------------------------------------------------'
  ' 1) clear dualport_return
  ' 2) clear play flag
  ' 3) 
  ' 3) set timer b to the remaining time untilk music
  if irq_mode = 1
  ayc_init = { $86, $00, $b7, dualport_return / 256, dualport_return mod 256, _
               $86, $00, $b7, flag_loc / 256, flag_loc mod 256, _
              $1c, $ef, _
              $86, $a0, $b7, $d0, $0e, _
              $cc, $0, $2, $fd, $d0, $08 }
  else
    ' in non-irq mode, we ignore the code that actually sets upo the IRQ - we're going
    ' to push in data a different way...
    'ayc_init = { $7c, dualport_status/256, dualport_status mod 256, _
    '  $86, $00, $b7, dualport_return / 256, dualport_return mod 256 }
    'ayc_init = { $86, $00, $b7, dualport_return / 256, dualport_return mod 256 }
    ayc_init = { $86, $01, $b7, dualport_status/256, dualport_status mod 256, _
      $86, $00, $b7, dualport_return / 256, dualport_return mod 256 }
  endif
  'ayc_init = { $cc, $30, $75, $fd, $d0, $08, $86, $00, $b7, dualport_return / 256, dualport_return mod 256 }

  '--------------------------------------------------------------------
  ' this resets the VIA at the end, so that wait_recal doens't wait - this should be the last thing you call.
	' note that, in VIA buffered mode, all it does is update dualport_status to the current sequence semaphore
	' without that, playback is.... weird due to locking...
  '--------------------------------------------------------------------
  ' first line is: lda #sequence, sta $dualport_status
  ' in betten turn of finterrupts again
	' second line is: ldd #$100, std $d008 (remember: endian is reversed)
	' which should set timer b to "almost nothing"
  if irq_mode = 1
  ayc_exit = { $86, ayc_dp_sequence, $b7, dualport_status / 256, dualport_status mod 256, _
                $1a, $10, _
                $86, $80, $b7, $d0, $0e, _
	  					 $cc, $1, $0, $fd, $d0, $08}
  else
  ayc_exit = { $86, ayc_dp_sequence, $b7, dualport_status / 256, dualport_status mod 256 }
  endif
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
  dim textlist[2, 3]
  textlist = {{200, -150, "50HZ AYC"},{200, -170, "FPS: "}}
  call ReturnToOriginSprite()
  call TextListSprite(textlist)
  if buffer_mode = 1
    call ReturnToOriginSprite()
    call ScaleSprite(32)
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
loop_start = 0
' globalize this ;)
ayc_tick = 0
while controls[1,3] = 0
  if demo_mode = 1
    fps = 960.0 / (GetTickCount() - loop_start)
    textlist[2, 3] = "FPS:"+Int(fps)+" AYC:"+ayc_tick
    loop_start = GetTickCount()
  endif
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

sub ayc_update_timer
  current_tick = GetTickCount() - ayc_start_time 
  music_target = (current_tick / 960.0) * player_rate + 1
  if ayc_buffer_overflow == true
    ayc_ticked = int(music_target)
    call Poke(flag_loc,0)
  endif
  to_tick = int(music_target) - ayc_ticked
  if to_tick > 0
    ayc_ticked = ayc_ticked + 1
    call Poke(flag_loc, Peek(flag_loc)+1)
  endif
endsub

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
  ayc_tick = GetTickCount()
  ayc_played_this_frame = 0
  if ayc_buffer_played >= 0 
    ' have a 1 second timeout on this - we've simplified the term since the '&1' could never ever be matched
    ' anyhow - the sequence either _is_, or _is not_.  If it _is not_, we wait at least 10 ticks for
    ' the first code to be executed.  We finally add a 1 second timeout - this should never ever get hit, but it'll cause
    ' us to break out...
		while ((Peek(dualport_status) != ayc_dp_sequence) or (GetTickCount()-ayc_tick)<10 and ayc_dp_sequence!=1) _
          and (GetTickCount()-ayc_tick)<960
      if irq_mode = 0
        call ayc_update_timer
      endif
		endwhile
		if Peek(dualport_status) != ayc_dp_sequence
      print "ohai, we didn't actuatlly update in "+(GetTickCount()-ayc_tick)+"... weird - dualport_status=" +Peek(dualport_status)+" expected "+ayc_dp_sequence
    endif
    'print "endframe"
    ' reset benchmark counter once we've synced ;)
    ayc_tick = GetTickCount()
		ayc_dp_sequence = (ayc_dp_sequence + 4) mod 256
		ayc_exit[2] = ayc_dp_sequence
	  ' fill any used buffers with new sound data
  	ayc_played_this_frame = Peek(dualport_return)
    if ayc_played_this_frame >= buffer_count
      print "WARN: AYC buffer limit of "+ayc_played_this_frame+" hit in "+(((GetTickCount()-lframe)/960.0)*1000.0)+" ms - consider increasing buffer size..."
      ayc_buffer_overflow = true
    endif
    lframe = GetTickCount()
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


  ' fix the IRQ timing
  if irq_mode = 1
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
    ayc_init[19] = wait_time mod 256
    ayc_init[20] = wait_time / 256
  endif
  ' benchmark
  w_tick = GetTickCount() - ayc_tick
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
    ' set up IRQ jmp
    call pokeRAM($cbf8, $7e)
    call pokeRAM($cbf9, (player_code_loc / 256) mod 256)
    call pokeRAM($cbfa, player_code_loc mod 256)
    call pokeRAM(flag_loc, 0)
    for j = 1 to Ubound(internal_ayc_playcode)
      call pokeRAM(player_code_loc+(j-1), internal_ayc_playcode[j])
    next
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
' final acceptable error: 4.9
' final angle tollerance: 0.170859375
' final command count: 415
sub  rose()
  call LinesSprite({ _
    { MoveTo , 14.5393231456 , -16.9427367259 }, _
    { DrawTo , -5.8079845672 , -27.6560418319 }, _
    { DrawTo , -22.6497085463 , -36.0983817897 }, _
    { DrawTo , -15.4833774136 , -38.8157762957 }, _
    { DrawTo , -6.67874175907 , -39.4931027 }, _
    { DrawTo , 5.76055248652 , -38.4274563421 }, _
    { DrawTo , 35.5240003501 , -32.0591507667 }, _
    { DrawTo , 54.3473758392 , -21.7583739789 }, _
    { DrawTo , 62.2578481843 , -12.1391806531 }, _
    { DrawTo , 64.5404028992 , -4.46534143712 }, _
    { DrawTo , 75.3167088082 , -3.9717188772 }, _
    { DrawTo , 80.8468345723 , -5.27019081566 }, _
    { DrawTo , 84.9460512233 , -21.0146904265 }, _
    { DrawTo , 79.2353655023 , -33.891425486 }, _
    { MoveTo , 60.4512846388 , -41.2479464982 }, _
    { DrawTo , 58.5879430581 , -46.2770115358 }, _
    { DrawTo , 47.5231840419 , -44.5042141815 }, _
    { DrawTo , 37.1552954605 , -36.5329092689 }, _
    { MoveTo , 35.5394043697 , -50.548796014 }, _
    { DrawTo , 38.1532832115 , -63.0481466571 }, _
    { DrawTo , 39.0544394244 , -95.6862019886 }, _
    { DrawTo , 38.0894702135 , -110.399244013 }, _
    { DrawTo , 52.3007644818 , -104.542235391 }, _
    { DrawTo , 60.0378902318 , -97.6034964195 }, _
    { DrawTo , 64.284197452 , -87.7063065085 }, _
    { DrawTo , 64.4522331235 , -75.8235952556 }, _
    { DrawTo , 60.8186587039 , -62.2898035495 }, _
    { DrawTo , 89.0320072378 , -57.1946770964 }, _
    { DrawTo , 115.831116964 , -43.2896161885 }, _
    { DrawTo , 124.261710583 , -37.4446251349 }, _
    { DrawTo , 140.954208436 , -25.7992523726 }, _
    { MoveTo , 154.836049775 , -22.8566439677 }, _
    { DrawTo , 152.076034213 , -54.362937323 }, _
    { DrawTo , 130.180969276 , -58.3163621871 }, _
    { DrawTo , 135.640509958 , -42.7192507007 }, _
    { DrawTo , 152.86790445 , -11.6968015921 }, _
    { DrawTo , 164.466148601 , -0.787080931015 }, _
    { DrawTo , 170.340214985 , -146.446196973 }, _
    { DrawTo , 171.617697068 , -230.310536512 }, _
    { DrawTo , 179.482933565 , -248.518265828 }, _
    { DrawTo , 185.387318173 , -256.765650275 }, _
    { DrawTo , 207.037455262 , -277.907963113 }, _
    { DrawTo , 234.022304922 , -294.312269318 }, _
    { DrawTo , 252.216627953 , -310.496615545 }, _
    { DrawTo , 256.0 , -316.135908335 }, _
    { DrawTo , 240.623359665 , -319.763779672 }, _
    { DrawTo , 231.083312402 , -319.32444076 }, _
    { DrawTo , 158.588163559 , -319.32444076 }, _
    { DrawTo , -146.087394413 , -319.32444076 }, _
    { DrawTo , -229.358849164 , -319.32444076 }, _
    { DrawTo , -250.923651789 , -319.461889691 }, _
    { DrawTo , -248.590693869 , -305.979097559 }, _
    { DrawTo , -238.739721424 , -297.990529825 }, _
    { DrawTo , -206.712442296 , -278.243420471 }, _
    { DrawTo , -184.886533869 , -255.75519832 }, _
    { DrawTo , -178.280236786 , -245.759230638 }, _
    { DrawTo , -170.4978206 , -222.783483782 }, _
    { DrawTo , -169.390408277 , -209.571204393 }, _
    { DrawTo , -169.595513888 , -178.814889427 }, _
    { DrawTo , -169.451209355 , -88.3296809764 }, _
    { DrawTo , -167.640006231 , -36.8340338909 }, _
    { DrawTo , -144.128066066 , -21.0388476255 }, _
    { DrawTo , -132.032152515 , -14.7218030324 }, _
    { DrawTo , -117.876984871 , 21.2530560216 } _
  })
  call ReturnToOriginSprite()
  call LinesSprite({ _
    {MoveTo, -117.876984871 , 21.2530560216 }, _
    { DrawTo , -92.7662327752 , 47.0303056484 }, _
    { DrawTo , -81.9291876874 , 54.7509744507 }, _
    { DrawTo , -76.5312380912 , 65.4216081789 }, _
    { DrawTo , -62.8159396616 , 60.5390851831 }, _
    { DrawTo , -53.0192979261 , 55.683045663 }, _
    { DrawTo , -36.3650069759 , 58.779405357 }, _
    { DrawTo , -21.666125716 , 55.8051639118 }, _
    { DrawTo , -5.01575342247 , 59.5363913692 }, _
    { DrawTo , 3.80122413943 , 59.2973044363 }, _
    { DrawTo , 20.4555150897 , 53.6511745594 }, _
    { MoveTo , 25.8377900591 , 79.3989981021 }, _
    { DrawTo , 24.5057836197 , 57.4308489957 }, _
    { DrawTo , 13.2589890834 , 36.9033966084 }, _
    { DrawTo , 6.25528289416 , 30.3302672986 }, _
    { DrawTo , -5.80027984439 , 23.605896846 }, _
    { DrawTo , -16.771723505 , 19.1770457919 }, _
    { DrawTo , -33.4260144552 , 11.3202813509 }, _
    { DrawTo , -58.8972829674 , 23.7447096884 }, _
    { DrawTo , -81.5177287345 , 16.9553764462 }, _
    { DrawTo , -92.184623081 , 18.7789190039 }, _
    { DrawTo , -109.839819992 , 24.9607426117 }, _
    { MoveTo , -78.4905664383 , 15.3972652959 }, _
    { DrawTo , -61.0633204551 , 1.41987537265 }, _
    { DrawTo , -39.301060504 , -23.5922960689 }, _
    { DrawTo , -24.8333602584 , -35.6675974909 }, _
    { DrawTo , -40.3781254145 , -47.339963754 }, _
    { DrawTo , -42.2279824479 , -52.8966412146 }, _
    { DrawTo , -41.3622739251 , -59.6392490288 }, _
    { DrawTo , -35.3057082164 , -70.4983487949 }, _
    { DrawTo , -24.8127929997 , -53.7673424079 }, _
    { DrawTo , -12.8530668108 , -40.512294397 }, _
    { MoveTo , -5.99541759601 , -41.2479464982 }, _
    { DrawTo , -0.462401672883 , -44.7541445933 }, _
    { DrawTo , 12.1373681478 , -48.2030569449 }, _
    { DrawTo , 23.3945076103 , -49.5394813311 }, _
    { DrawTo , 35.5394043697 , -50.548796014 }, _
    { MoveTo , 46.9064477754 , -44.9262070043 }, _
    { DrawTo , 37.1098060399 , -50.0757717129 }, _
    { MoveTo , 12.6182017013 , -49.3401196116 }, _
    { DrawTo , 13.9867925518 , -80.1977826495 }, _
    { DrawTo , 6.36815539304 , -85.9014785935 }, _
    { DrawTo , -6.28147953469 , -92.3720892726 }, _
    { DrawTo , -4.79569600809 , -100.398769298 }, _
    { DrawTo , 2.82155996589 , -112.606200317 }, _
    { DrawTo , 11.6386866922 , -102.020275677 }, _
    { DrawTo , 14.9710152171 , -90.7635894228 }, _
    { DrawTo , 13.5978658749 , -82.4444641666 }, _
    { MoveTo , 11.6385375278 , -104.514027203 }, _
    { DrawTo , 13.8414204971 , -109.264364523 }, _
    { DrawTo , 13.2588358413 , -125.918900132 }, _
    { DrawTo , 11.6385375278 , -137.618371758 }, _
    { MoveTo , 20.3026874786 , -133.203723499 }, _
    { DrawTo , 37.1098060399 , -110.399244013 }, _
    { MoveTo , 46.9064477754 , -127.750334472 }, _
    { DrawTo , 10.6588733542 , -149.388805378 }, _
    { DrawTo , 20.3026874786 , -133.203723499 }, _
    { MoveTo , 9.6792091807 , -149.388805378 }, _
    { DrawTo , -1.43736756239 , -161.30925365 }, _
    { DrawTo , -4.58003272837 , -166.555441623 }, _
    { DrawTo , -10.3181164575 , -183.54312755 }, _
    { DrawTo , -10.9178588671 , -189.299418681 }, _
    { DrawTo , -9.03727485487 , -203.827060868 }, _
    { DrawTo , -2.86730069457 , -214.539263126 }, _
    { DrawTo , 4.80668984121 , -220.807880347 } _
  })
  call ReturnToOriginSprite()
  call CodeSprite(ayc_playcode)
  call LinesSprite({ _
    {MoveTo, 4.80668984121 , -220.807880347 }, _
    { DrawTo , 10.5931007648 , -222.800958575 }, _
    { DrawTo , -19.7107160256 , -226.632276006 }, _
    { MoveTo , -29.1772109346 , -219.275019342 }, _
    { DrawTo , -9.78279929093 , -238.402709626 }, _
    { DrawTo , 0.228388898515 , -248.701839043 }, _
    { DrawTo , -7.21549926977 , -257.739379556 }, _
    { DrawTo , -16.8840950733 , -261.931536182 }, _
    { DrawTo , -21.6700443727 , -263.838616677 }, _
    { DrawTo , -28.6153270158 , -267.986327975 }, _
    { DrawTo , -23.1058694659 , -275.601911106 }, _
    { DrawTo , -13.9028970508 , -279.27333771 }, _
    { DrawTo , 8.01740370202 , -280.801718993 }, _
    { DrawTo , 24.3741717839 , -280.334879395 }, _
    { DrawTo , 155.649171039 , -280.334879395 }, _
    { DrawTo , 206.591708063 , -278.127923092 }, _
    { MoveTo , 188.957752939 , -287.691400407 }, _
    { DrawTo , 220.307006493 , -286.220096205 }, _
    { MoveTo , 116.462604097 , -287.691400407 }, _
    { DrawTo , 188.957752939 , -287.691400407 }, _
    { MoveTo , 138.015215915 , -203.573996545 }, _
    { DrawTo , 168.384805295 , -186.90706254 }, _
    { MoveTo , 167.405141121 , -168.51576001 }, _
    { DrawTo , 144.388823325 , -156.077504173 }, _
    { DrawTo , 125.552539196 , -141.238946383 }, _
    { DrawTo , 117.17677928 , -133.940111252 }, _
    { MoveTo , 105.941105679 , -123.284032205 }, _
    { DrawTo , 114.113569807 , -111.890312121 }, _
    { DrawTo , 126.028045087 , -103.778375102 }, _
    { DrawTo , 145.958825513 , -75.3119433051 }, _
    { DrawTo , 149.771185998 , -58.1679448263 }, _
    { MoveTo , 126.259245832 , -56.6966406239 }, _
    { DrawTo , 114.314091881 , -77.1882616349 }, _
    { DrawTo , 110.093240605 , -82.2032511248 }, _
    { DrawTo , 73.9638189908 , -110.960849238 }, _
    { DrawTo , 46.9064477754 , -127.750334472 }, _
    { MoveTo , 60.621746205 , -122.905329734 }, _
    { DrawTo , 74.9161349762 , -123.64897027 }, _
    { DrawTo , 80.3786646688 , -123.148879547 }, _
    { DrawTo , 103.726969841 , -119.962721329 }, _
    { MoveTo , 107.645626535 , -66.995770041 }, _
    { DrawTo , 94.9848859625 , -80.1400494549 }, _
    { DrawTo , 89.3090071102 , -84.2237441294 }, _
    { DrawTo , 73.5739253432 , -92.1349933984 }, _
    { DrawTo , 65.5200670727 , -94.2148977862 }, _
    { MoveTo , 108.625290709 , -66.2601179397 }, _
    { DrawTo , 124.299917485 , -58.1679448263 }, _
    { MoveTo , 104.706634014 , -52.2827280165 }, _
    { DrawTo , 108.625290709 , -66.2601179397 }, _
    { MoveTo , 63.5607387256 , -15.5001229554 }, _
    { DrawTo , 70.9119617649 , -20.4216427726 }, _
    { DrawTo , 79.2353655023 , -30.2131649799 }, _
    { MoveTo , 55.7234253373 , -22.1209918664 }, _
    { DrawTo , 59.3229761001 , -27.8872037192 }, _
    { DrawTo , 60.7355356901 , -38.6863218735 }, _
    { MoveTo , 4.78088831297 , -47.8688154092 }, _
    { DrawTo , -4.16356167401 , -62.726303888 }, _
    { DrawTo , -19.7107160256 , -48.6044675104 }, _
    { MoveTo , -43.2226561907 , -83.8532379405 }, _
    { DrawTo , -32.2053343481 , -90.1111863417 }, _
    { DrawTo , -24.3155029888 , -99.5176201703 }, _
    { DrawTo , -20.4138530095 , -114.83731861 }, _
    { DrawTo , -20.674127659 , -120.701774873 }, _
    { DrawTo , -25.4495887543 , -136.882719657 }, _
    { DrawTo , -22.4655316816 , -148.653153277 } _
  })
  call ReturnToOriginSprite()
  call LinesSprite({ _
    {MoveTo, -22.4655316816 , -148.653153277 }, _
    { DrawTo , -25.9303339798 , -153.255019551 }, _
    { DrawTo , -35.8771342175 , -161.435108535 }, _
    { DrawTo , -42.2429920171 , -169.987064212 }, _
    { MoveTo , -45.1819845378 , -178.079237325 }, _
    { DrawTo , -43.169639032 , -182.584271179 }, _
    { DrawTo , -41.4543623574 , -192.79227935 }, _
    { DrawTo , -29.1772109346 , -219.275019342 }, _
    { MoveTo , -44.929231181 , -241.345318031 }, _
    { DrawTo , -38.01378178 , -251.164066626 }, _
    { DrawTo , -18.8215963868 , -252.508534933 }, _
    { DrawTo , -0.117432554747 , -250.908795346 }, _
    { DrawTo , 31.4040940442 , -245.700958675 }, _
    { DrawTo , 45.5734770618 , -241.823858599 }, _
    { DrawTo , 57.6827536844 , -237.892167067 }, _
    { DrawTo , 138.015215915 , -203.573996545 }, _
    { MoveTo , 1.84189579234 , -113.341852418 }, _
    { DrawTo , -2.3557233253 , -125.770252526 }, _
    { DrawTo , -5.08129762857 , -130.207250845 }, _
    { DrawTo , -18.6983323643 , -144.898670264 }, _
    { MoveTo , -41.2633278436 , -139.825328062 }, _
    { DrawTo , -37.3446711494 , -160.423586896 }, _
    { MoveTo , -52.7714370697 , -166.554553635 }, _
    { DrawTo , -60.8626183715 , -174.753246239 }, _
    { DrawTo , -59.1903438603 , -188.977754028 }, _
    { DrawTo , -54.5450267982 , -187.438735459 }, _
    { DrawTo , -44.9633597067 , -176.990768294 }, _
    { MoveTo , -58.8972829674 , -189.849670945 }, _
    { DrawTo , -73.6706187044 , -233.253144917 }, _
    { DrawTo , -75.4369532093 , -248.622388616 }, _
    { DrawTo , -65.8431019578 , -253.179017731 }, _
    { DrawTo , -50.0803054055 , -261.935484692 }, _
    { DrawTo , -30.4870219346 , -268.564445776 }, _
    { MoveTo , -19.7107160256 , -278.127923092 }, _
    { DrawTo , -24.9694125633 , -279.963539125 }, _
    { DrawTo , -47.696158092 , -281.373446911 }, _
    { DrawTo , -71.6329172235 , -280.334879395 }, _
    { DrawTo , -204.867244825 , -279.599227294 }, _
    { MoveTo , -219.562207429 , -286.220096205 }, _
    { DrawTo , -213.564953768 , -287.70582315 }, _
    { DrawTo , -190.172282222 , -287.691400407 }, _
    { DrawTo , -124.534782595 , -287.691400407 }, _
    { DrawTo , 116.462604097 , -287.691400407 }, _
    { MoveTo , -63.7956038351 , -253.11575165 }, _
    { DrawTo , -41.2633278436 , -251.644447448 }, _
    { MoveTo , -63.7956038351 , -208.976625577 }, _
    { DrawTo , -55.5640314385 , -220.800879169 }, _
    { DrawTo , -44.929231181 , -241.345318031 }, _
    { MoveTo , -118.244358937 , -198.67749616 }, _
    { DrawTo , -154.636510855 , -232.360182438 }, _
    { DrawTo , -167.829413781 , -236.49722741 }, _
    { MoveTo , -168.619670404 , -172.929672617 }, _
    { DrawTo , -148.046685008 , -187.809772934 }, _
    { DrawTo , -142.208604733 , -193.929069397 }, _
    { DrawTo , -129.433103463 , -209.712277678 }, _
    { MoveTo , -92.0961424805 , -146.446196973 }, _
    { DrawTo , -97.2708496274 , -165.553406466 }, _
    { DrawTo , -104.877059741 , -179.606550752 }, _
    { DrawTo , -118.244358937 , -198.67749616 }, _
    { MoveTo , -91.2262006944 , -144.97489277 }, _
    { DrawTo , -62.8159396616 , -137.103415287 }, _
    { DrawTo , -35.3853428023 , -139.825328062 }, _
    { MoveTo , -40.791129712 , -129.526198645 }, _
    { DrawTo , -26.5683652404 , -139.825328062 }, _
    { MoveTo , -56.0876061177 , -116.284460823 } _
  })
  call ReturnToOriginSprite()
  call CodeSprite(ayc_playcode)
  call LinesSprite({ _
    {MoveTo, -56.0876061177 , -116.284460823 }, _
    { DrawTo , -40.791129712 , -129.526198645 }, _
    { MoveTo , -52.5079132275 , -103.778375102 }, _
    { DrawTo , -56.0876061177 , -116.284460823 }, _
    { MoveTo , -50.0803054055 , -82.4444641666 }, _
    { DrawTo , -52.5079132275 , -103.778375102 }, _
    { MoveTo , -119.636461727 , -102.373279589 }, _
    { DrawTo , -101.370444221 , -120.154927214 }, _
    { DrawTo , -92.468392768 , -140.164630833 }, _
    { MoveTo , -141.189073545 , -88.3296809764 }, _
    { DrawTo , -117.77925496 , -91.1226554079 }, _
    { DrawTo , -109.672920285 , -93.3555748401 }, _
    { DrawTo , -58.8972829674 , -114.81315662 }, _
    { MoveTo , -134.33142433 , -78.0305515593 }, _
    { DrawTo , -43.2226561907 , -83.8532379405 }, _
    { MoveTo , -136.711028608 , -54.4896843202 }, _
    { DrawTo , -134.33142433 , -78.0305515593 }, _
    { MoveTo , -152.945043628 , -86.858376774 }, _
    { DrawTo , -119.636461727 , -102.373279589 }, _
    { MoveTo , -168.619670404 , -88.3296809764 }, _
    { DrawTo , -156.226256643 , -86.3386060057 }, _
    { DrawTo , -150.934212785 , -84.6686735135 }, _
    { DrawTo , -135.311088504 , -78.7662036605 }, _
    { MoveTo , -133.351760157 , -18.4427313603 }, _
    { DrawTo , -136.711028608 , -54.4896843202 }, _
    { MoveTo , -165.719276652 , 2.15552747387 }, _
    { DrawTo , -166.660342057 , -36.0983817897 }, _
    { MoveTo , -81.4295589589 , 53.6511745594 }, _
    { DrawTo , -71.8721963257 , 45.9673004868 }, _
    { DrawTo , -56.3450597644 , 37.8598320028 }, _
    { DrawTo , -53.3546997395 , 31.5955774794 }, _
    { DrawTo , -40.6430022593 , 29.3466261604 }, _
    { DrawTo , -33.4260144552 , 30.5333072785 }, _
    { DrawTo , -1.9894037039 , 29.8347650489 }, _
    { DrawTo , 21.4351792632 , 22.9752175905 }, _
    { DrawTo , 36.1301418664 , 14.6616131946 }, _
    { DrawTo , 14.5393231456 , -16.9427367259 }, _
    { MoveTo , 51.8047686431 , 38.938132535 }, _
    { DrawTo , 36.1987183585 , 13.9259610934 }, _
    { MoveTo , 51.5382999879 , 40.1975689322 }, _
    { DrawTo , 74.3370446346 , 46.2946535472 }, _
    { DrawTo , 65.5200670727 , -3.7296893359 }, _
    { MoveTo , 75.3167088082 , 47.0303056484 }, _
    { DrawTo , 90.9913355849 , 48.5016098508 }, _
    { DrawTo , 119.401596618 , 44.0854902872 }, _
    { DrawTo , 160.547491906 , 27.9033510166 }, _
    { MoveTo , 159.44047139 , 49.9729140533 }, _
    { DrawTo , 165.476182364 , -0.0514288297939 }, _
    { MoveTo , 152.710178518 , -12.5575145506 }, _
    { DrawTo , 154.836049775 , -22.8566439677 }, _
    { MoveTo , 90.9002268167 , 55.8581308631 }, _
    { DrawTo , 93.5414014286 , 49.3660010698 }, _
    { MoveTo , 82.6857427216 , 67.6285644826 }, _
    { DrawTo , 90.9002268167 , 55.8581308631 }, _
    { MoveTo , 63.5146945095 , 66.1572602802 }, _
    { DrawTo , 50.6200061466 , 40.8348313739 }, _
    { MoveTo , 57.6827536844 , 79.3989981021 }, _
    { DrawTo , 73.0159928013 , 75.5939189869 }, _
    { DrawTo , 79.232785708 , 70.8662362755 }, _
    { MoveTo , 56.4454378332 , 79.8161128435 }, _
    { DrawTo , 60.0003469396 , 76.4788302101 }, _
    { DrawTo , 63.1607957841 , 69.9204900732 }, _
    { MoveTo , 47.8861119489 , 80.8703023046 }, _
    { DrawTo , 40.3537305853 , 62.196061521 }, _
    { DrawTo , 32.5821047798 , 54.1872747251 } _
  })
  call ReturnToOriginSprite()
  call LinesSprite({ _
    {MoveTo, 32.5821047798 , 54.1872747251 }, _
    { DrawTo , 26.2547751322 , 51.2544598694 }, _
    { MoveTo , 1.84189579234 , 74.9850854948 }, _
    { DrawTo , 9.82310152841 , 78.0629236514 }, _
    { DrawTo , 24.3741717839 , 74.2494333936 }, _
    { MoveTo , 0.862231618798 , 74.2494333936 }, _
    { DrawTo , -3.27489018608 , 61.3269685835 }, _
    { MoveTo , -20.2105618581 , 64.4659800979 }, _
    { DrawTo , -27.1881919987 , 66.0502277271 }, _
    { DrawTo , -32.2365004346 , 66.2918935044 }, _
    { DrawTo , -49.8141836167 , 66.1643735428 }, _
    { DrawTo , -71.6143036042 , 70.7499363481 }, _
    { DrawTo , -69.9350027788 , 75.8483708033 }, _
    { DrawTo , -65.8062834086 , 80.2418666016 }, _
    { DrawTo , -59.954997743 , 83.3079865272 }, _
    { DrawTo , -53.9237524613 , 84.5103524119 }, _
    { MoveTo , -56.9379546203 , 68.3642165838 }, _
    { DrawTo , -54.3511695059 , 80.1268230685 }, _
    { DrawTo , -50.1990062008 , 84.5189594254 }, _
    { DrawTo , -43.4913804642 , 87.6759428433 }, _
    { DrawTo , -29.7503144761 , 77.9276938997 }, _
    { DrawTo , -35.3853428023 , 67.6285644826 }, _
    { MoveTo , -27.548029414 , 78.6633460009 }, _
    { DrawTo , -7.27192001414 , 87.4382042643 }, _
    { DrawTo , 1.84189579234 , 74.9850854948 }, _
    { MoveTo , 8.69954500715 , 104.411169544 }, _
    { DrawTo , 19.2031003876 , 105.628358061 }, _
    { DrawTo , 41.0284627341 , 99.9972569364 }, _
    { DrawTo , 47.2519543589 , 85.7528396969 }, _
    { MoveTo , 46.7692947911 , 124.273776277 }, _
    { DrawTo , 41.9620826915 , 99.9972569364 }, _
    { MoveTo , 50.8251044696 , 133.837253593 }, _
    { DrawTo , 63.4509068983 , 132.011600211 }, _
    { DrawTo , 69.635706277 , 130.154572223 }, _
    { DrawTo , 88.0523430642 , 122.508431929 }, _
    { DrawTo , 127.196739164 , 97.3906767503 }, _
    { DrawTo , 154.865023052 , 61.4156045299 }, _
    { DrawTo , 159.44047139 , 49.9729140533 }, _
    { MoveTo , 48.970600189 , 134.367953018 }, _
    { DrawTo , 46.7692947911 , 124.273776277 }, _
    { MoveTo , 37.1098060399 , 138.2511662 }, _
    { DrawTo , 48.970600189 , 134.367953018 }, _
    { MoveTo , 27.3131643045 , 135.617310982 }, _
    { DrawTo , 13.5978658749 , 138.008768833 }, _
    { DrawTo , 2.82155996589 , 134.005865054 }, _
    { DrawTo , -12.9412365864 , 138.127429516 }, _
    { DrawTo , -21.6700443727 , 135.625256025 }, _
    { DrawTo , -35.3853428023 , 138.986818301 }, _
    { DrawTo , -44.8904245624 , 135.922502917 }, _
    { DrawTo , -62.8159396616 , 132.053150117 }, _
    { DrawTo , -100.043178256 , 116.602396165 }, _
    { DrawTo , -132.240044974 , 92.4881947038 }, _
    { DrawTo , -155.273784061 , 55.9474624033 }, _
    { DrawTo , -160.340967871 , 39.9173798366 }, _
    { DrawTo , -165.719276652 , 2.15552747387 }, _
    { MoveTo , -135.311088504 , 84.5485628107 }, _
    { DrawTo , -76.5312380912 , 68.3642165838 }, _
    { MoveTo , -53.0192979261 , 29.3746552191 }, _
    { DrawTo , -58.8972829674 , 24.2250905105 }, _
    { MoveTo , -25.5887010669 , 55.8581308631 }, _
    { DrawTo , -33.2909696349 , 52.7782460374 }, _
    { DrawTo , -56.749859099 , 46.2313874665 }, _
    { MoveTo , -21.6700443727 , 106.618125847 }, _
    { DrawTo , -35.3902492457 , 96.2467620662 }, _
    { MoveTo , -19.1885550211 , 110.265488965 } _
  })
  call ReturnToOriginSprite()
  call CodeSprite(ayc_playcode)
  call LinesSprite({ _
    {MoveTo, -19.1885550211 , 110.265488965 }, _
    { DrawTo , -15.2073953186 , 113.25578504 }, _
    { DrawTo , 3.13113384473 , 106.348141526 }, _
    { DrawTo , 24.2790950651 , 84.9420109936 }, _
    { MoveTo , 0.862231618798 , 105.882473746 }, _
    { DrawTo , -15.6065269649 , 105.777895532 }, _
    { MoveTo , -12.8530668108 , 147.814643516 }, _
    { DrawTo , -27.8703389271 , 158.113772933 }, _
    { DrawTo , -32.3924687522 , 170.619858654 }, _
    { DrawTo , -30.4519647816 , 178.377272016 }, _
    { DrawTo , -25.6437059333 , 184.403970452 }, _
    { DrawTo , -21.1906328466 , 187.466999098 }, _
    { DrawTo , -15.9814469861 , 189.770928083 }, _
    { DrawTo , -10.2235263317 , 191.319800195 }, _
    { DrawTo , -1.01140694239 , 192.236220328 }, _
    { DrawTo , 5.211072035 , 191.91513746 }, _
    { DrawTo , 14.1479446282 , 190.045575696 }, _
    { DrawTo , 24.2342613956 , 184.983234724 }, _
    { DrawTo , 29.5195852917 , 179.276972231 }, _
    { DrawTo , 32.0734586094 , 171.949150113 }, _
    { DrawTo , 30.1841173051 , 161.210814965 }, _
    { DrawTo , 24.4561124001 , 154.001130043 }, _
    { DrawTo , 13.5978658749 , 147.814643516 }, _
    { DrawTo , 4.87932793927 , 151.848073478 }, _
    { DrawTo , -11.9736390764 , 148.421415226 }, _
    { DrawTo , -12.8530668108 , 138.986818301 }, _
    { MoveTo , -25.5887010669 , 153.699860326 }, _
    { DrawTo , -35.2403525047 , 146.342750792 }, _
    { DrawTo , -38.2556842486 , 138.497194705 }, _
    { MoveTo , 13.5978658749 , 147.814643516 }, _
    { DrawTo , 13.5978658749 , 138.986818301 }, _
    { MoveTo , 27.3131643045 , 152.228556123 } _
  })
endsub
