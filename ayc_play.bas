'---------------------------
' GLOBALS - these must be global 
' for your music to play!

' this needs to be the size of your filename, however I don't have a way to get that
' in gsbasic that I know of!@
dim ay_buffer_data[7400]
' this might need to be 1024 for some things....
dim comp_buffer[14,1024]
dim cursor[14]
' our data file
dim ay_buffer_sizes[15]
dim ay_buffer_offsets[15]
dim actual_size[15]
' set  some player constants
dim last_flags[14]
dim comp_flags[14]
dim coffset[14]
dim poffset[14]
dim premaining[14]

'-------------------------------------------
' Everything below here can go in a function
'-------------------------------------------

' load the AYC file
fh = fopen("switchblade.ayc", "rb")
foffset = 0

ayc_duration = fgetc(fh) + fgetc(fh)*256
foffset = foffset + 2
header_offset = 0
print "Duration ",ayc_duration," frames"
for reg = 1 to 14
  ay_buffer_sizes[reg] = fgetc(fh)
  ay_buffer_offsets[reg] = fgetc(fh) + fgetc(fh)*256 
  ' correct for relative offset
  ay_buffer_offsets[reg] = ay_buffer_offsets[reg]+(reg-1)*3+4
  if reg = 1
    header_offset = ay_buffer_offsets[reg] 
  endif
  ' gsbasic fix
  ay_buffer_offsets[reg] = ay_buffer_offsets[reg] - header_offset
  ' init our flags here too
  last_flags[reg] = 0
  comp_flags[reg] = 0
  poffset[reg] = 0
  coffset[reg] = 0
  cursor[reg] = 0
  premaining[reg] = 0
  print "Reg ",reg," buffer size ",ay_buffer_sizes[reg]," @ ",ay_buffer_offsets[reg]
  foffset = foffset + 3
next
' workaround for no flen again :)
ay_buffer_offsets[15] = 9999999

boffset = 1
fl = 0
for reg = 1 to 14
  print "Skipping from ",foffset," to ",ay_buffer_offsets[reg]+header_offset, "(boffset=",boffset,")"
  while foffset != (ay_buffer_offsets[reg]+header_offset)
    call fgetc(fh)
    foffset = foffset+1
  endwhile
  ' now read the buffer data - until next buffer offset or EOF
  while foffset < (ay_buffer_offsets[reg+1]+header_offset) and (feof(fh) =0)
    d = fgetc(fh)
    if reg = 2 and fl = 0
      print d
      fl = 1
    endif
    foffset = foffset + 1
    ay_buffer_data[boffset] = d
    boffset = boffset + 1
  endwhile
  actual_size[reg] = boffset - ay_buffer_offsets[reg]
  ' this fixes later the gsbasic's dim is alays start at 1 thing
  ay_buffer_offsets[reg] = ay_buffer_offsets[reg] + 1
next

print "Read ",foffset,"bytes sucessfully"
  

call SetFrameRate(50)
controls = WaitForFrame(JoystickNone, Controller2, JoystickNone)
call TextSprite("AYC TEST")

frame_count = 0
while controls[1,3] = 0
  max_regs = 14
  dim outregs[max_regs, 2]

  controls = WaitForFrame(JoystickNone, Controller2, JoystickNone)
  call play_that_music
endwhile


sub play_that_music
  frame_count = frame_count + 1
  if frame_count < ayc_duration
    ' play loop
    for reg = 1 to max_regs
      ' if we're not in a pattern, process next byte
      if premaining[reg] = 0
        ' if the high bit is set, this is direct data - otherwise it is
        ' a sequence pointer
        if comp_flags[reg] < 128
          ' we're direct data - shove it in the AY!
          'print reg," reading from ",ay_buffer_offsets[reg] + coffset[reg]
          my_data = ay_buffer_data[ay_buffer_offsets[reg] + coffset[reg]]
          coffset[reg] = coffset[reg] + 1
          outregs[reg, 1] = reg - 1
          outregs[reg, 2] = my_data
        
          ' shove it in the buffer so we can get it back...
          comp_buffer[reg, cursor[reg]+1] = my_data
          if reg=3
            print "data = ",my_data," in reg ",reg," cursor ",cursor[reg]+1," coffset ",coffset[reg]," flags ",comp_flags[reg]
          endif
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
          if reg=3
            print "FAIL: ",reg," tries to set poffset of ",poffset[reg], " coffset=",coffset[reg], " flags=",comp_flags[reg]
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
        comp_flags[reg] = (comp_flags[reg] * 2) mod 256
        ' every 8 frames, we grab new flags
        if last_flags[reg] = 0
          comp_flags[reg] = ay_buffer_data[ay_buffer_offsets[reg] + coffset[reg]]
          coffset[reg] = coffset[reg] + 1
          print reg," -> read flags ",comp_flags[reg]
        endif
        ' This should be an 'and 7', but i had weirdness with that!
        last_flags[reg] = (last_flags[reg] + 1) mod 8  
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
        my_data = comp_buffer[reg, poffset[reg]+1]
        
        ' shove it in the buffer so we can get it back
        comp_buffer[reg, cursor[reg]+1] = my_data
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
  endif
endsub
