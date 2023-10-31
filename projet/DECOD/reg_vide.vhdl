library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Reg is
port(
	-- Write Port 1 prioritaire
		wdata1		: in Std_Logic_Vector(31 downto 0);
		wadr1			: in Std_Logic_Vector(3 downto 0);
		wen1			: in Std_Logic;

	-- Write Port 2 non prioritaire
		wdata2		: in Std_Logic_Vector(31 downto 0);
		wadr2			: in Std_Logic_Vector(3 downto 0);
		wen2			: in Std_Logic;

	-- Write CSPR Port
		wcry			: in Std_Logic;
		wzero			: in Std_Logic;
		wneg			: in Std_Logic;
		wovr			: in Std_Logic;
		cspr_wb		: in Std_Logic;
		
	-- Read Port 1 32 bits
		reg_rd1		: out Std_Logic_Vector(31 downto 0);
		radr1			: in Std_Logic_Vector(3 downto 0);
		reg_v1		: out Std_Logic;

	-- Read Port 2 32 bits
		reg_rd2		: out Std_Logic_Vector(31 downto 0);
		radr2			: in Std_Logic_Vector(3 downto 0);
		reg_v2		: out Std_Logic;

	-- Read Port 3 32 bits
		reg_rd3		: out Std_Logic_Vector(31 downto 0);
		radr3			: in Std_Logic_Vector(3 downto 0);
		reg_v3		: out Std_Logic;

	-- read CSPR Port
		reg_cry		: out Std_Logic;
		reg_zero		: out Std_Logic;
		reg_neg		: out Std_Logic;
		reg_cznv		: out Std_Logic;
		reg_ovr		: out Std_Logic;
		reg_vv		: out Std_Logic;
		
	-- Invalidate Port 
		inval_adr1	: in Std_Logic_Vector(3 downto 0);
		inval1		: in Std_Logic;

		inval_adr2	: in Std_Logic_Vector(3 downto 0);
		inval2		: in Std_Logic;

		inval_czn	: in Std_Logic;
		inval_ovr	: in Std_Logic;

	-- PC
		reg_pc		: out Std_Logic_Vector(31 downto 0);
		reg_pcv		: out Std_Logic;
		inc_pc		: in Std_Logic;
	
	-- global interface
		ck				: in Std_Logic;
		reset_n		: in Std_Logic;
		vdd			: in bit;
		vss			: in bit);
end Reg;


architecture Behavior OF Reg is

signal reg0:std_logic_vector(31 downto 0);
signal reg1:std_logic_vector(31 downto 0);
signal reg2:std_logic_vector(31 downto 0);
signal reg3:std_logic_vector(31 downto 0);
signal reg4:std_logic_vector(31 downto 0);
signal reg5:std_logic_vector(31 downto 0);
signal reg6:std_logic_vector(31 downto 0);
signal reg7:std_logic_vector(31 downto 0);
signal reg8:std_logic_vector(31 downto 0);
signal reg9:std_logic_vector(31 downto 0);
signal reg10:std_logic_vector(31 downto 0);
signal reg11:std_logic_vector(31 downto 0);
signal reg12:std_logic_vector(31 downto 0);
signal reg13:std_logic_vector(31 downto 0);
signal reg14:std_logic_vector(31 downto 0);
signal regpc:std_logic_vector(31 downto 0);

signal vali0:std_logic;
signal vali1:std_logic;
signal vali2:std_logic;
signal vali3:std_logic;
signal vali4:std_logic;
signal vali5:std_logic;
signal vali6:std_logic;
signal vali7:std_logic;
signal vali8:std_logic;
signal vali9:std_logic;
signal vali10:std_logic;
signal vali11:std_logic;
signal vali12:std_logic;
signal vali13:std_logic;
signal vali14:std_logic;
signal valipc:std_logic;

signal cry:std_logic;
signal zero:std_logic;
signal neg:std_logic;
signal ovr:std_logic;

signal vali_czn:std_logic;
signal vali_v:std_logic;


begin

process(ck)
variable reg_pc_var:integer;
begin
	if reset_n = '0' then
		vali0<='1';
		vali1<='1';
		vali2<='1';
		vali3<='1';
		vali4<='1';
		vali5<='1';
		vali6<='1';
		vali7<='1';
		vali8<='1';
		vali9<='1';
		vali10<='1';
		vali11<='1';
		vali12<='1';
		vali13<='1';
		vali14<='1';
		valipc<='1';
		vali_czn<='1';
		vali_v<='1';

	elsif rising_edge(ck) then

		--registre 0 ecriture
		if vali0='0' then
			if (wen1='1' and wadr1="0000") then reg0<=wdata1; vali0<='1';
			elsif (wen2='1' and wadr2="0000") then reg0<=wdata2; vali0<='1';
			end if;
		end if;

		--registre 1 ecriture
		if vali1='0' then
			if (wen1='1' and wadr1="0001") then reg1<=wdata1; vali1<='1';
			elsif (wen2='1' and wadr2="0001") then reg1<=wdata2; vali1<='1';
			end if;
		end if;


		--registre 2 ecriture
		if vali2='0' then
			if (wen1='1' and wadr1="0010") then reg2<=wdata1; vali2<='1';
			elsif (wen2='1' and wadr2="0010") then reg2<=wdata2; vali2<='1';
			end if;
		end if;


		--registre 3 ecriture
		if vali3='0' then
			if (wen1='1' and wadr1="0011") then reg3<=wdata1; vali3<='1';
			elsif (wen2='1' and wadr2="0011") then reg3<=wdata2; vali3<='1';
			end if;
		end if;


		--registre 4 ecriture
		if vali4='0' then
			if (wen1='1' and wadr1="0100") then reg4<=wdata1; vali4<='1';
			elsif (wen2='1' and wadr2="0100") then reg4<=wdata2; vali4<='1';
			end if;
		end if;


		--registre 5 ecriture
		if vali5='0' then
			if (wen1='1' and wadr1="0101") then reg5<=wdata1; vali5<='1';
			elsif (wen2='1' and wadr2="0101") then reg5<=wdata2; vali5<='1';
			end if;
		end if;


		--registre 6 ecriture
		if vali6='0' then
			if (wen1='1' and wadr1="0110") then reg6<=wdata1; vali6<='1';
			elsif (wen2='1' and wadr2="0110") then reg6<=wdata2; vali6<='1';
			end if;
		end if;


		--registre 7 ecriture
		if vali7='0' then
			if (wen1='1' and wadr1="0111") then reg7<=wdata1; vali7<='1';
			elsif (wen2='1' and wadr2="0111") then reg7<=wdata2; vali7<='1';
			end if;
		end if;


		--registre 8 ecriture
		if vali8='0' then
			if (wen1='1' and wadr1="1000") then reg8<=wdata1; vali8<='1';
			elsif (wen2='1' and wadr2="1000") then reg8<=wdata2; vali8<='1';
			end if;
		end if;


		--registre 9 ecriture
		if vali9='0' then
			if (wen1='1' and wadr1="1001") then reg9<=wdata1; vali9<='1';
			elsif (wen2='1' and wadr2="1001") then reg9<=wdata2; vali9<='1';
			end if;
		end if;


		--registre 10 ecriture
		if vali10='0' then
			if (wen1='1' and wadr1="1010") then reg10<=wdata1; vali10<='1';
			elsif (wen2='1' and wadr2="1010") then reg10<=wdata2; vali10<='1';
			end if;
		end if;


		--registre 11 ecriture
		if vali11='0' then
			if (wen1='1' and wadr1="1011") then reg11<=wdata1; vali11<='1';
			elsif (wen2='1' and wadr2="1011") then reg11<=wdata2; vali11<='1';
			end if;
		end if;


		--registre 12 ecriture
		if vali12='0' then
			if (wen1='1' and wadr1="1100") then reg12<=wdata1; vali12<='1';
			elsif (wen2='1' and wadr2="1100") then reg12<=wdata2; vali12<='1';
			end if;
		end if;


		--registre 13 ecriture
		if vali13='0' then
			if (wen1='1' and wadr1="1101") then reg13<=wdata1; vali13<='1';
			elsif (wen2='1' and wadr2="1101") then reg13<=wdata2; vali13<='1';
			end if;
		end if;


		--registre 14 ecriture
		if vali14='0' then
			if (wen1='1' and wadr1="1110") then reg14<=wdata1; vali14<='1';
			elsif (wen2='1' and wadr2="1110") then reg14<=wdata2; vali14<='1';
			end if;
		end if;


		--registre pc ecriture
		if valipc='1' then
			if (inc_pc='1') then reg_pc_var:=to_integer(signed(regpc));
					     reg_pc_var:=reg_pc_var+4;
					     regpc<=std_logic_vector(to_unsigned(reg_pc_var,32));
					     valipc<='1';
			end if;
		end if;

		if valipc='0' then
			if (wen1='1' and wadr1="1111") then regpc<=wdata1; valipc<='1';
			elsif (wen2='1' and wadr2="1111") then regpc<=wdata2; valipc<='1';
			end if;
		end if;


	


	--cspr
	if cspr_wb='1' then
		if vali_czn='0' then
			cry<=wcry;
			zero<=wzero;
			neg<=wneg;
			vali_czn<='1';
		end if;
		

		if vali_v='0' then
			ovr<=wovr;
			vali_v<='1';
		end if; 
	end if;





	--invali->vali0
	if (inval_adr1="0000" and inval1='1') then
		vali0<='0';
	end if;

	if (inval_adr2="0000" and inval2='1') then
		vali0<='0';
	end if;

	--invali->vali1
	if (inval_adr1="0001" and inval1='1') then
		vali1<='0';
	end if;

	if (inval_adr2="0001" and inval2='1') then
		vali1<='0';
	end if;

	--invali->vali2
	if (inval_adr1="0010" and inval1='1') then
		vali2<='0';
	end if;

	if (inval_adr2="0010" and inval2='1') then
		vali2<='0';
	end if;

	--invali->vali3
	if (inval_adr1="0011" and inval1='1') then
		vali3<='0';
	end if;

	if (inval_adr2="0011" and inval2='1') then
		vali3<='0';
	end if;

	--invali->vali4
	if (inval_adr1="0100" and inval1='1') then
		vali4<='0';
	end if;

	if (inval_adr2="0100" and inval2='1') then
		vali4<='0';
	end if;

	--invali->vali5
	if (inval_adr1="0101" and inval1='1') then
		vali5<='0';
	end if;

	if (inval_adr2="0101" and inval2='1') then
		vali5<='0';
	end if;

	--invali->vali6
	if (inval_adr1="0110" and inval1='1') then
		vali6<='0';
	end if;

	if (inval_adr2="0110" and inval2='1') then
		vali6<='0';
	end if;

	--invali->vali7
	if (inval_adr1="0111" and inval1='1') then
		vali7<='0';
	end if;

	if (inval_adr2="0111" and inval2='1') then
		vali7<='0';
	end if;

	--invali->vali8
	if (inval_adr1="1000" and inval1='1') then
		vali8<='0';
	end if;

	if (inval_adr2="1000" and inval2='1') then
		vali8<='0';
	end if;

	--invali->vali9
	if (inval_adr1="1001" and inval1='1') then
		vali9<='0';
	end if;

	if (inval_adr2="1001" and inval2='1') then
		vali9<='0';
	end if;

	--invali->vali10
	if (inval_adr1="1010" and inval1='1') then
		vali10<='0';
	end if;

	if (inval_adr2="1010" and inval2='1') then
		vali10<='0';
	end if;

	--invali->vali11
	if (inval_adr1="1010" and inval1='1') then
		vali11<='0';
	end if;

	if (inval_adr2="1010" and inval2='1') then
		vali11<='0';
	end if;

	--invali->vali12
	if (inval_adr1="1011" and inval1='1') then
		vali12<='0';
	end if;
	if (inval_adr2="1011" and inval2='1') then
		vali12<='0';
	end if;

	--invali->vali13
	if (inval_adr1="1100" and inval1='1') then
		vali13<='0';
	end if;

	if (inval_adr2="1100" and inval2='1') then
		vali13<='0';
	end if;

	--invali->vali14
	if (inval_adr1="1101" and inval1='1') then
		vali14<='0';
	end if;

	if (inval_adr2="1101" and inval2='1') then
		vali14<='0';
	end if;

	--invali->vali15
	if (inval_adr1="1111" and inval1='1') then
		valipc<='0';
	end if;

	if (inval_adr2="1111" and inval2='1') then
		valipc<='0';
	end if;


	--invali->czn
	if inval_czn='1' then
		vali_czn<='0';
	end if;

	--inval->vv
	if inval_ovr='1' then
		vali_v<='0';
	end if;

end if;

end process;


--lecture rd1
reg_rd1<=reg0 when radr1="0000" else reg1 when radr1="0001" else reg2 when radr1="0010" else reg3 when radr1="0011" else reg4 when radr1="0100" else reg5 when radr1="0101"
else reg6 when radr1="0110" else reg7 when radr1="0111" else reg8 when radr1="1000" else reg9 when radr1="1001" else reg10 when radr1="1010" else reg11 when radr1="1011"
else reg12 when radr1="1100" else reg13 when radr1="1101" else reg14 when radr1="1110";


--validite rd1
reg_v1<=vali0 when radr1="0000" else vali1 when radr1="0001" else vali2 when radr1="0010" else vali3 when radr1="0011" else vali4 when radr1="0100" else vali5 when radr1="0101"
else vali6 when radr1="0110" else vali7 when radr1="0111" else vali8 when radr1="1000" else vali9 when radr1="1001" else vali10 when radr1="1010" else vali11 when radr1="1011"
else vali12 when radr1="1100" else vali13 when radr1="1101" else vali14 when radr1="1110" else valipc when radr1="1111";


--lecture rd2
reg_rd2<=reg0 when radr2="0000" else reg1 when radr2="0001" else reg2 when radr2="0010" else reg3 when radr2="0011" else reg4 when radr2="0100" else reg5 when radr2="0101"
else reg6 when radr2="0110" else reg7 when radr2="0111" else reg8 when radr2="1000" else reg9 when radr2="1001" else reg10 when radr2="1010" else reg11 when radr2="1011"
else reg12 when radr2="1100" else reg13 when radr2="1101" else reg14 when radr2="1110" else regpc when radr2="1111";		


--validite rd2
reg_v2<=vali0 when radr2="0000" else vali1 when radr2="0001" else vali2 when radr2="0010" else vali3 when radr2="0011" else vali4 when radr2="0100" else vali5 when radr2="0101"
else vali6 when radr2="0110" else vali7 when radr2="0111" else vali8 when radr2="1000" else vali9 when radr2="1001" else vali10 when radr2="1010" else vali11 when radr2="1011"
else vali12 when radr2="1100" else vali13 when radr2="1101" else vali14 when radr2="1110" else valipc when radr1="1111";		
	

--lecture rd3
reg_rd3<=reg0 when radr3="0000" else reg1 when radr3="0001" else reg2 when radr3="0010" else reg3 when radr3="0011" else reg4 when radr3="0100" else reg5 when radr3="0101"
else reg6 when radr3="0110" else reg7 when radr3="0111" else reg8 when radr3="1000" else reg9 when radr3="1001" else reg10 when radr3="1010" else reg11 when radr3="1011"
else reg12 when radr3="1100" else reg13 when radr3="1101" else reg14 when radr3="1110" else regpc when radr3="1111";		


--validite rd3
reg_v3<=vali0 when radr3="0000" else vali1 when radr3="0001" else vali2 when radr3="0010" else vali3 when radr3="0011" else vali4 when radr3="0100" else vali5 when radr3="0101"
else vali6 when radr3="0110" else vali7 when radr3="0111" else vali8 when radr3="1000" else vali9 when radr3="1001" else vali10 when radr3="1010" else vali11 when radr3="1011"
else vali12 when radr3="1100" else vali13 when radr3="1101" else vali14 when radr3="1110" else valipc when radr1="1111";


--lecture cspr czn
reg_cry<=cry;
reg_zero<=zero;
reg_neg<=neg;
reg_ovr<=ovr;
reg_cznv<=vali_czn;
reg_vv<=vali_v;


--lecture reg_pc
reg_pc<=regpc;

--validite pc
reg_pcv<=valipc;






end Behavior;
