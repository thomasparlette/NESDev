--emu.speedmode("normal") -- St the speed of the emulator



-- Declare and set variables or functions if needed

timing = false;

lastcount = 0;
nowcount = 0;
timingvalue = 0;

function calcframes(address, size)
    
    value = memory.readbyte(address);
    if value == 2 then
        lastcount = debugger.getcyclescount();
        return
    elseif value == 1 then
        now = debugger.getcyclescount();
        diff = now - lastcount;
        gui.text(10,10,diff);
    end
end



memory.registerwrite(0x07FF, 1, calcframes);
while true do
    emu.frameadvance() -- This essentially talls FCEUX to keep running

end

