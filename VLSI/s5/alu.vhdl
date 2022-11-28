LIBRARY ieee;
USE ieee.std_logic_1164.all;
--USE ieee.numeric_std.all;

entity Alu is
    port (  op1 : in Std_Logic_Vector(31 downto 0);
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
end Alu;

architecture archi of Alu is
    signal resultat : Std_Logic_Vector(31 downto 0):=x"00000000";--& '0';
    --signal command : std_logic_Vector(1 downto 0);
    --signal add_resultat :std_logic_Vector(32 downto 0):=x"00000000" ;
    --command <= cmd;
    begin
    alu_add:entity work.add32
        port map(op1,op2,cin,cout,resultat);
    res<=resultat when cmd="00" else
        op1 and op2 when cmd="01" else
        op1 or op2 when cmd= "10" else
        op1 xor op2 when cmd="11";
        --case (command) is
            --when "00" => 
                --alu_add:entity work.add32
                    --port map(op1,op2,cin,cout,resultat); 
            --when "01" =>
                --resultat <='0' & op1 and op2;
            --when "10" =>
                --resultat <='0' & op1 or op2;
            --when "11" =>
                --resultat <='0' & op1 xor op2;
            --when others => NULL;
                --end case;        
    --z      <='1' when res=0 else '0';
    --n      <='1' when res(31)='1'else '0';
    --v      <='1' when ((resultat(31)='0')and (res!=0))else '0';
    --boucle:
    --if res =0 then z<='1';
    --else if res(31)='1' then n<='1';
    --else if ((res(31)='0')and (res!=0)) then v<='1';
    

        

                    
end archi;