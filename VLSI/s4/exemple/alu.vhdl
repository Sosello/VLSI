library ieee;
use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;

entity alu is
port ( op1 : in Std_Logic_Vector(31 downto 0);
op2 : in Std_Logic_Vector(31 downto 0);
cin : in Std_Logic;
cmd : in Std_Logic_Vector(1 downto 0);
res : out Std_Logic_Vector(31 downto 0);
cout : out Std_Logic;
z : out Std_Logic;
n : out Std_Logic;
v : out Std_Logic;
vdd : in bit;
vss : in bit );
end alu;

architecture archi of alu is
signal result : std_Logic_Vector(31 downto 0);
signal carry : std_logic_vector(32 downto 0):=x"00000000" & '0';

begin
carry(0) <= cin;
cout <= carry(32);

boucle : for i in 0 to 31 generate
my_adder : entity work.add1
port map(a => op1(i),
b => op2(i),
cin => carry(i),
s => result(i),
cout => carry(i+1)
);
end generate;

res <= result when cmd="00"
else (op1 and op2) when cmd="01"
else (op1 or op2) when cmd="10"
else (op1 xor op2);


--z <= '1' when result=0;
-- n <= '1' when result(31)='1';

end archi;