library ieee;
use ieee.std_logic_1164.all;

entity simu is
end simu;

architecture testbench of simu is
signal shift_lsl   : Std_Logic;
signal shift_lsr   : Std_Logic;
signal shift_asr   : Std_Logic;
signal shift_ror   : Std_Logic;
signal shift_rrx   : Std_Logic;
signal shift_val   : Std_Logic_Vector(4 downto 0);
signal din         : Std_Logic_Vector(31 downto 0);
signal cin         : Std_Logic;
signal dout        : Std_Logic_Vector(31 downto 0);
signal cout        : Std_Logic;

--global interface
signal vdd         : bit;
signal vss         : bit;

begin
my_shifter:
entity work.Shifter 
port map(shift_lsl,shift_lsr,shift_asr,shift_ror,shift_rrx,shift_val,din,cin,dout,cout,vdd,vss);
din <=x"012a3b4d";
cin <='1', '0' after 10 ns;
shift_lsl<='0', '1' after 10 ns,'0' after 20 ns;
shift_lsr<='0', '1' after 20 ns,'0' after 30 ns;
shift_asr<='0', '1' after 30 ns,'0' after 40 ns;
shift_ror<='0', '1' after 40 ns,'0' after 50 ns;
shift_rrx<='0', '1' after 50 ns,'0' after 60 ns;

end testbench;