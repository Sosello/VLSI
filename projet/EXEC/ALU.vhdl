library ieee;
use ieee.std_logic_1164.all;

entity ALU is
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
		vss : in bit);
end ALU;

architecture Behavior of ALU is
signal resultat : std_logic_vector(31 downto 0);
signal c: std_logic_vector(32 downto 0):="00000000000000000000000000000000" & '0';

component adder
    port (A, B : in std_logic;
		C : in std_logic;
		S : out std_logic;
		Cout : out std_logic);
end component;


begin
	c(0)<=cin;
	cout<=c(32);
	boucle: for i in 0 to 31 generate
				adder32: adder port map(
					A=>op1(i),
					B=>op2(i),
					C=>c(i),
					s=>resultat(i),
					cout=>c(i+1)
					);
			end generate;

		res <= resultat when cmd="00" 
		else (op1 AND op2) when cmd="01"	
		else (op1 OR op2) when cmd="10" 
		else (op1 XOR op2) ;--when cmd="11";
		--else NULL when others;
	n<='1' when resultat(31)='1' else '0' ;--when resultat(31)='0';
	z<='1' when resultat=x"00000000" else '0' ;--when resultat/=x"00000000";
	v<='1' when c(32)='1' else '0' ;--when c(32)='0';



end Behavior;
