library ieee;
use ieee.std_logic_1164.all;

entity Shifter is
    port(
        shift_lsl   :in Std_Logic;
        shift_lsr   :in Std_Logic;
        shift_asr   :in Std_Logic;
        shift_ror   :in Std_Logic;
        shift_rrx   :in Std_Logic;
        shift_val   :in Std_Logic_Vector(4 downto 0);
        din         :in Std_Logic_Vector(31 downto 0);
        cin         :in Std_Logic;
        dout        :out Std_Logic_Vector(31 downto 0);
        cout        :out Std_Logic;

--global interface
        vdd         :in bit;
        vss         :in bit);
end Shifter;

architecture archi of Shifter is
signal intermediaire: Std_Logic_Vector(31 downto 0);
begin
intermediaire <= cin& din(31 downto 1) when shift_lsr='1' else
                 din(30 downto 0)&cin when shift_lsl='1' else
                 din(31)&din(31 downto 1)when shift_asr='1' else
                 din(0)&din(31 downto 1)when shift_ror='1' else
                 cin&din(31 downto 1)when shift_rrx ='1';
cout <= intermediaire(31) when shift_lsl='1' else
        intermediaire(0) when shift_lsr='1' else
        intermediaire(0) when shift_asr='1' else
        intermediaire(0) when shift_ror='1' else
        intermediaire(0) when shift_rrx='1' ;

dout <= intermediaire;






end archi;
