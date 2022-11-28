LIBRARY ieee;
USE ieee.std_logic_1164.all;
--USE ieee.numeric_std.all;

ENTITY add32 IS
    PORT(
        i0:in std_logic_Vector(31 downto 0);
        i1:in std_logic_Vector(31 downto 0);
        cin:in std_logic;
        cout : out std_logic;
        q:out std_logic_Vector(31 downto 0)
    );
END ENTITY;


ARCHITECTURE nomarchi OF add32 IS
    SIGNAL c : std_logic_vector(30 downto 0):="0000000000000000000000000000000";
    BEGIN
        bit0:   entity work.add1
                port map(i0(0),i1(0),cin,c(0),q(0));
        bit1:   entity work.add1
                port map(i0(1),i1(1),c(0),c(1),q(1));
        bit2:   entity work.add1
                port map(i0(2),i1(2),c(1),c(2),q(2));
        bit3:   entity work.add1
                port map(i0(3),i1(3),c(2),c(3),q(3));
        bit4:   entity work.add1
                port map(i0(4),i1(4),c(3),c(4),q(4));
        bit5:   entity work.add1
                port map(i0(5),i1(5),c(4),c(5),q(5));
        bit6:   entity work.add1
                port map(i0(6),i1(6),c(5),c(6),q(6));
        bit7:   entity work.add1
                port map(i0(7),i1(7),c(6),c(7),q(7));
        bit8:   entity work.add1
                port map(i0(8),i1(8),c(7),c(8),q(8));
        bit9:   entity work.add1
                port map(i0(9),i1(9),c(8),c(9),q(9));
        bit10:   entity work.add1
                port map(i0(10),i1(10),c(9),c(10),q(10));
        bit11:   entity work.add1
                port map(i0(11),i1(11),c(10),c(11),q(11));
        bit12:   entity work.add1
                port map(i0(12),i1(12),c(11),c(12),q(12));
        bit13:   entity work.add1
                port map(i0(13),i1(13),c(12),c(13),q(13));
        bit14:   entity work.add1
                port map(i0(14),i1(14),c(13),c(14),q(14));
        bit15:   entity work.add1
                port map(i0(15),i1(15),c(14),c(15),q(15));
        bit16:   entity work.add1
                port map(i0(16),i1(16),c(15),c(16),q(16));
        bit17:   entity work.add1
                port map(i0(17),i1(17),c(16),c(17),q(17));
        bit18:   entity work.add1
                port map(i0(18),i1(18),c(17),c(18),q(18));
        bit19:   entity work.add1
                port map(i0(19),i1(19),c(18),c(19),q(19));
        bit20:   entity work.add1
                port map(i0(20),i1(20),c(19),c(20),q(20));
        bit21:   entity work.add1
                port map(i0(21),i1(21),c(20),c(21),q(21));
        bit22:   entity work.add1
                port map(i0(22),i1(22),c(21),c(22),q(22));
        bit23:   entity work.add1
                port map(i0(23),i1(23),c(22),c(23),q(23));
        bit24:   entity work.add1
                port map(i0(24),i1(24),c(23),c(24),q(24));
        bit25:   entity work.add1
                port map(i0(25),i1(25),c(24),c(25),q(25));
        bit26:   entity work.add1
                port map(i0(26),i1(26),c(25),c(26),q(26));
        bit27:   entity work.add1
                port map(i0(27),i1(27),c(26),c(27),q(27));
        bit28:   entity work.add1
                port map(i0(28),i1(28),c(27),c(28),q(28));
        bit29:   entity work.add1
                port map(i0(29),i1(29),c(28),c(29),q(29));
        bit30:   entity work.add1
                port map(i0(30),i1(30),c(29),c(30),q(30));
        bit31:   entity work.add1
                port map(i0(31),i1(31),c(30),cout,q(31));        
        
        
END nomarchi;