library ieee;
use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;

entity SIMU is
end SIMU;

architecture archi of SIMU is

signal op1 : Std_Logic_Vector(31 downto 0) := (others => '0');
signal op2 : Std_Logic_Vector(31 downto 0) := (others => '0');
signal cin : Std_Logic:= '0';
signal cmd : Std_Logic_Vector(1 downto 0):= (others => '0');
signal res : Std_Logic_Vector(31 downto 0):= (others => '0');
signal cout : Std_Logic:= '0';
signal z : Std_Logic:= '0';
signal n : Std_Logic:= '0';
signal v : Std_Logic:= '0';
signal vdd : bit:= '0';
signal vss : bit:= '0';
begin
my_alu : entity work.alu
port map(
op1 => op1,
op2 => op2,
cin => cin,
cmd => cmd,
res => res,
cout => cout,
z => z,
n => n,
vdd => vdd,
vss => vss);

cmd <= "01" after 10 ns, "00" after 20 ns, "11" after 30 ns;
op1 <= x"00000001" after 8 ns, x"00000003" after 16 ns, x"0000000f" after 25 ns;
op2 <= x"00000002" after 8 ns, x"00000004" after 16 ns, x"00000003" after 25 ns;

end archi;