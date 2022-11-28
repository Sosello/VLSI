LIBRARY ieee;
USE ieee.std_logic_1164.all;
--USE ieee.numeric_std.all;

ENTITY add4 IS
    PORT(
        i0:in std_logic_Vector(3 downto 0);
        i1:in std_logic_Vector(3 downto 0);
        cin:in std_logic;
        cout : out std_logic;
        q:out std_logic_Vector(3 downto 0)
    );
END ENTITY;


ARCHITECTURE nomarchi OF add4 IS
    SIGNAL c : std_logic_vector(2 downto 0):="000";
    BEGIN
        bit0:   entity work.add1
                port map(i0(0),i1(0),cin,c(0),q(0));
        bit1:   entity work.add1
                port map(i0(1),i1(1),c(0),c(1),q(1));
        bit2:   entity work.add1
                port map(i0(2),i1(2),c(1),c(2),q(2));
        bit3:   entity work.add1
                port map(i0(3),i1(3),c(2),cout,q(3));
        
END nomarchi;