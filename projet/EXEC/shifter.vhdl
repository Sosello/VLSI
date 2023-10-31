library ieee;
use ieee.std_logic_1164.all;

entity Shifter is
port(
shift_lsl: in Std_Logic;
shift_lsr: in Std_Logic;
shift_asr: in Std_Logic;
shift_ror: in Std_Logic;
shift_rrx: in Std_Logic;
shift_val : in Std_Logic_Vector(4 downto 0);

din: in Std_Logic_Vector(31 downto 0);
cin: in Std_Logic;

dout: out Std_Logic_Vector(31 downto 0);
cout: out Std_Logic;

-- global interface
vdd: in bit;
vss: in bit);
end Shifter;

architecture dataflow of Shifter is
signal dout_0: std_logic_vector(31 downto 0);
signal dout_1: std_logic_vector(31 downto 0);
signal dout_2: std_logic_vector(31 downto 0);
signal dout_3: std_logic_vector(31 downto 0);

signal cout_gauche: std_logic;
signal cout_droite: std_logic;

begin
	dout_0<=din(30 downto 0) & '0'	when shift_lsl='1' and shift_val(0)='1' 
	else    '0' & din(31 downto 1)  when shift_lsr='1' and shift_val(0)='1'
	else     din(31) & din(31 downto 1) when shift_asr='1' and shift_val(0)='1'
	else     din(0) & din(31 downto 1) when shift_ror='1' and shift_val(0)='1'
	else	 cin & din(31 downto 1) when shift_rrx='1' and shift_val(0)='1'
	else din;

	dout_1<=dout_0(29 downto 0) & "00"      when shift_lsl='1' and shift_val(1)='1' 
	else    "00" & dout_0(31 downto 2)  when shift_lsr='1' and shift_val(1)='1'
	else     din(31) & dout_0(31) & din(31 downto 2) when shift_asr='1' and shift_val(1)='1'
	else     din(0) & dout_0(31 downto 1) when shift_ror='1' and shift_val(1)='1'
	else	 cin & cin & dout_0(31 downto 2) when shift_rrx='1' and shift_val(1)='1'	
	else dout_0;

	dout_2<=dout_1(27 downto 0) & "0000"    when shift_lsl='1' and shift_val(2)='1' 
	else    "0000" & dout_1(31 downto 4)  when shift_lsr='1' and shift_val(2)='1'
	else     din(31) & din(31) & din(31) & din(31) & dout_1(31 downto 4) when shift_asr='1' and shift_val(2)='1'
	else     din(3 downto 0) & dout_1(31 downto 4)  when shift_ror='1' and shift_val(2)='1'
	else	 cin & cin & cin & cin & dout_1(31 downto 4) when shift_rrx='1' and shift_val(2)='1'	
	else dout_1;

	dout_3<=dout_2(23 downto 0) & "00000000" when shift_lsl='1' and shift_val(3)='1' 
	else    "00000000" & dout_2(31 downto 8)  when shift_lsr='1' and shift_val(3)='1'
	else     din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) &  dout_2(31 downto 8) when shift_asr='1' and shift_val(3)='1'
	else     din(7 downto 0) & dout_2(31 downto 8)  when shift_ror='1' and shift_val(3)='1'
	else	 cin & cin & cin & cin & cin & cin & cin & cin & dout_2(31 downto 8) when shift_rrx='1' and shift_val(3)='1'
	else dout_2;

	dout<=dout_3(15 downto 0) & "0000000000000000" when shift_lsl='1' and shift_val(4)='1' 
	else     "0000000000000000" & dout_3(31 downto 16)  when shift_lsr='1' and shift_val(4)='1'
	else     din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & dout_3(31 downto 16)     when 		shift_asr='1' and shift_val(4)='1'
	else     din(15 downto 0) & dout_3(31 downto 16)  when shift_ror='1' and shift_val(4)='1'
	else	 cin & cin & cin & cin & cin & cin & cin & cin & cin & cin & cin & cin & cin & cin & cin & cin & dout_3(31 downto 16) when shift_rrx='1' and shift_val(4)='1'
	else dout_3;



--	dout(31 downto 0)<='0' & din(31 downto 1) 		when shift_val(0)='1'
--	else dout(31 downto 0)<="00" & din(31 downto 2)          when shift_val(1)='1'
--	else dout(31 downto 0)<="0000" & din(31 downto 4)            when shift_val(2)='1'
--	else dout(31 downto 0)<="00000000" & din(31 downto 8)     when shift_val(3)='1'
--	else dout(31 downto 0)<="0000000000000000" & din(31 downto 16)     when shift_val(4)='1';



--	dout(31 downto 0)<=din(31) & din(31 downto 1) 		when shift_val(0)='1'
--	else dout(31 downto 0)<=din(31) & din(31) & din(31 downto 2)          when shift_val(1)='1'
--	else dout(31 downto 0)<=din(31) & din(31) & din(31) & din(31) & din(31 downto 4)            when shift_val(2)='1'
--	else dout(31 downto 0)<=din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) &  din(31 downto 8)     when shift_val(3)='1'
--	else dout(31 downto 0)<=din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31) & din(31 downto --16)     when shift_val(4)='1';


--	dout(31 downto 0)<=din(0) & din(31 downto 1) 		when shift_val(0)='1'
--	else dout(31 downto 0)<=din(1 downto 0) & din(31 downto 2)          when shift_val(1)='1'
--	else dout(31 downto 0)<=din(3 downto 0) & din(31 downto 4)            when shift_val(2)='1'
--	else dout(31 downto 0)<=din(7 downto 0) & din(31 downto 8)     when shift_val(3)='1'
--	else dout(31 downto 0)<=din(15 downto 0) & din(31 downto 16)     when shift_val(4)='1';

	--if(shift_rrx='1') then
	
	cout_gauche<='1' when (din(31)='1' and shift_val="00001")
	or (din(30)='1' and shift_val(4 downto 1)="0001")
	or (din(28)='1' and shift_val(4 downto 2)="001")
	or (din(24)='1' and shift_val(4 downto 3)="01")
	or (din(16)='1' and shift_val(4)='1')
	else '0';

	cout_droite<='1' when (din(0)='1' and shift_val="00001")
	or (din(1)='1' and shift_val(4 downto 1)="0001")
	or (din(3)='1' and shift_val(4 downto 2)="001")
	or (din(7)='1' and shift_val(4 downto 3)="01")
	or (din(15)='1' and shift_val(4)='1')
	else '0';
	
	cout<=cout_gauche when shift_lsl='1'
	else cout_droite when shift_lsr='1' or shift_asr='1' or shift_ror='1' or shift_rrx='1';
	


end;
