'---------------------------
' GLOBALS - these must be global 
' for your music to play!
' TODO: possibly refactor this to be a struct of an ayc_data type, so it can be passed
' instead of global
'---------------------------
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
max_regs = 14


' load the AYC
call load_and_init("switchblade.ayc")

' set the framerate to 50fps, since that is what most AYC tracks are
call SetFrameRate(50)

' play!
controls = WaitForFrame(JoystickNone, Controller2, JoystickNone)
call TextSprite("AYC TEST")

while controls[1,3] = 0
  controls = WaitForFrame(JoystickNone, Controller2, JoystickNone)
  call play_that_music
endwhile

sub load_and_init(filename)
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
  ay_buffer_data = fread(100000, fh)

  ' fix the buffer pointers?
  for reg = 1 to 14
    ' now read the buffer data - until next buffer offset or EOF
    ' this fixes later the gsbasic's dim is alays start at 1 thing
    ay_buffer_offsets[reg] = ay_buffer_offsets[reg] + 1
  next

  print "AYC: Read ",ftell(fh),"bytes sucessfully"
endsub

sub restart_music
    ' init our flags here too
  for reg = 1 to max_regs
    last_flags[reg] = 0
    comp_flags[reg] = 0
    poffset[reg] = 0
    coffset[reg] = 0
    cursor[reg] = 0
    premaining[reg] = 0
  next
endsub

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
        my_data = ay_buffer_data[ay_buffer_offsets[reg] + coffset[reg]]
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
        premaining[reg] = 256-ay_buffer_data[ay_buffer_offsets[reg] + coffset[reg]]
        coffset[reg] = coffset[reg] + 1

        ' depending on size, 8 or 16bits for offset
        poffset[reg] = ay_buffer_data[ay_buffer_offsets[reg] + coffset[reg]] 
        coffset[reg] = coffset[reg] + 1
        if ay_buffer_sizes[reg] != 1
          poffset[reg] = poffset[reg] + ay_buffer_data[ay_buffer_offsets[reg] + coffset[reg]] * 256
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
        comp_flags[reg] = ay_buffer_data[ay_buffer_offsets[reg] + coffset[reg]]
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
  call Sound(outregs)
endsub
