library ieee;
use ieee.std_logic_1164.all;

entity adder is
port (A, B : in std_logic;
		C : in std_logic;
		S : out std_logic;
		Cout : out std_logic);
end adder;

architecture Behavior of adder is

begin
	PROCESS (A, B, C)
	BEGIN

		Cout <= (C AND (A XOR B)) OR (A AND B);
		S <= A XOR B XOR C;
	END PROCESS;

end Behavior;
