LIBRARY ieee;
USE ieee.std_logic_1164.all;
--USE ieee.numeric_std.all;

ENTITY simu IS
    
END simu;


ARCHITECTURE testbench OF simu IS
    SIGNAL  i0:std_logic_Vector(3 downto 0) ;
    SIGNAL  i1:std_logic_Vector(3 downto 0) ;
    SIGNAL  cin:std_logic;
    SIGNAL  cout : std_logic;
    SIGNAL  q:std_logic_Vector(3 downto 0);
    begin
        adder4 : entity work.add4
                port map(i0,i1,cin,cout,q);
        i0 <= "0110" , "1000" after 20 ns, "0000" after 30 ns; 
        i1 <= "1010" , "0111" after 20 ns, "0000" after 30 ns;
        cin<= '1';
END testbench;