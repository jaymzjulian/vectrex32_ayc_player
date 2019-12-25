'---------------------------
' GLOBALS - these must be global 
' for your music to play!
' TODO: possibly refactor this to be a struct of an ayc_data type, so it can be passed
' instead of global
'---------------------------
' if this is 1, we call Sound, if this is 0, we call FillBuffer, and
' play via the codesprite
buffer_mode = 1
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

'--------------------------------------------------------------------'
' This is only required if buffer_mode is set to 1 - you can save a few byres of ram by excluding it if you want, otherwise
' number of buffers
buffer_count = 1
' rate of playback - 50hz by default...
player_rate = 50
current_buffer = 0
dim ayc_pokedata[max_regs*5*buffer_count]
dim ay_output_data[buffer_count, max_regs*2]
' should be in hex format, but for now this is what we get!
'ayc_playcode = { $FE, $C8, $82, $BD, $F2, $7D, $fc, $c8, $82, $c3, $00, $1d, $10, $83, $ca, $54, $2f, $03, $cc, $c8, $84, $fd, $c8, $82}
ayc_playcode = { $FE, $C8, $82, $BD, $F2, $7D }
'--------------------------------------------------------------------'

' you'll need to allow for max_regs*buffer_count worth of iram at this location 
' if this is the only weird thing you're using, c882 should be fine.  c880 is better, but doens't work
' on all v32 firmware revisions right now....
'
' Also, you'll need to update the asm block for this ;).  Eventually, that will happen automatically, but
' I have not coded this yet
buffer_location = $c882
buffer_base = $c884
' This is almost certainly wrong/destructive!
' but is.... probably enough?
dualport_return = 8


' load the AYC
call load_and_init("switchblade.ayc")


' set the framerate to 50fps, since that is what most AYC tracks are
call SetFrameRate(50)

' play!
controls = WaitForFrame(JoystickNone, Controller1, JoystickNone)
call TextSprite("AYC TEST")
' This creates the codesprite objects that we need
' send to the AY if it's time
call CodeSprite(ayc_playcode)


while controls[1,3] = 0
  'dim pd[2]
  'call Peek($c882, 16, pd)
  controls = WaitForFrame(JoystickNone, Controller1, JoystickNone)
  'print "frame:" + played_frames
  call play_that_music
  'while pd[1] = 0
  'endwhile
  'data = pd[2]
  'print data

endwhile

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
