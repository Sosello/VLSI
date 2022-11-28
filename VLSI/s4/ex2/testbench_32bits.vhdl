LIBRARY ieee;
USE ieee.std_logic_1164.all;
--USE ieee.numeric_std.all;

ENTITY simu IS
    
END simu;


ARCHITECTURE testbench OF simu IS
    SIGNAL  i0:std_logic_Vector(31 downto 0) ;
    SIGNAL  i1:std_logic_Vector(31 downto 0) ;
    SIGNAL  cin:std_logic;
    SIGNAL  cout : std_logic;
    SIGNAL  q:std_logic_Vector(31 downto 0);
    begin
        adder32 : entity work.add32
                port map(i0,i1,cin,cout,q);
        i0 <= "01100110011001100110011001100110" , "10001000100010001000100010001000" after 20 ns, "00000000000000000000000000000000" after 30 ns; 
        i1 <= "10101010101010101010101010101010" , "01110111011101110111011101110111" after 20 ns, "00000000000000000000000000000000" after 30 ns;
        cin<= '1';
END testbench;