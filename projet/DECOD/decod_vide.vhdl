library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Decod is
	port(
	-- Exec  operands
			dec_op1			: out Std_Logic_Vector(31 downto 0); -- first alu input
			dec_op2			: out Std_Logic_Vector(31 downto 0); -- shifter input
			dec_exe_dest	: out Std_Logic_Vector(3 downto 0); -- Rd destination
			dec_exe_wb		: out Std_Logic; -- Rd destination write back
			dec_flag_wb		: out Std_Logic; -- CSPR modifiy

	-- Decod to mem via exec
			dec_mem_data	: out Std_Logic_Vector(31 downto 0); -- data to MEM
			dec_mem_dest	: out Std_Logic_Vector(3 downto 0);
			dec_pre_index 	: out Std_logic;

			dec_mem_lw		: out Std_Logic;
			dec_mem_lb		: out Std_Logic;
			dec_mem_sw		: out Std_Logic;
			dec_mem_sb		: out Std_Logic;

	-- Shifter command
			dec_shift_lsl	: out Std_Logic;
			dec_shift_lsr	: out Std_Logic;
			dec_shift_asr	: out Std_Logic;
			dec_shift_ror	: out Std_Logic;
			dec_shift_rrx	: out Std_Logic;
			dec_shift_val	: out Std_Logic_Vector(4 downto 0);
			dec_cy			: out Std_Logic;

	-- Alu operand selection
			dec_comp_op1	: out Std_Logic;
			dec_comp_op2	: out Std_Logic;
			dec_alu_cy 		: out Std_Logic;

	-- Exec Synchro
			dec2exe_empty	: out Std_Logic;
			exe_pop			: in Std_logic;

	-- Alu command
			--dec_alu_add		: out Std_Logic;
			--dec_alu_and		: out Std_Logic;
			--dec_alu_or		: out Std_Logic;
			--dec_alu_xor		: out Std_Logic;
			dec_alu_cmd			: out std_logic;

	-- Exe Write Back to reg
			exe_res			: in Std_Logic_Vector(31 downto 0);

			exe_c				: in Std_Logic;
			exe_v				: in Std_Logic;
			exe_n				: in Std_Logic;
			exe_z				: in Std_Logic;

			exe_dest			: in Std_Logic_Vector(3 downto 0); -- Rd destination
			exe_wb			: in Std_Logic; -- Rd destination write back
			exe_flag_wb		: in Std_Logic; -- CSPR modifiy

	-- Ifetch interface
			dec_pc			: out Std_Logic_Vector(31 downto 0) ;
			if_ir				: in Std_Logic_Vector(31 downto 0) ;

	-- Ifetch synchro
			dec2if_empty	: out Std_Logic;
			if_pop			: in Std_Logic;

			if2dec_empty	: in Std_Logic;
			dec_pop			: out Std_Logic;

	-- Mem Write back to reg
			mem_res			: in Std_Logic_Vector(31 downto 0);
			mem_dest			: in Std_Logic_Vector(3 downto 0);
			mem_wb			: in Std_Logic;
			
	-- global interface
			ck					: in Std_Logic;
			reset_n			: in Std_Logic;
			vdd				: in bit;
			vss				: in bit);
end Decod;

----------------------------------------------------------------------

architecture Behavior OF Decod is

signal Rn : std_logic_vector(3 downto 0);
signal Rd : std_logic_vector(3 downto 0);
signal Rs : std_logic_vector(3 downto 0);
signal Rm : std_logic_vector(3 downto 0);
signal operand2 : std_logic_vector(11 downto 0);

signal dec_shift_lsl_inter: Std_Logic;
signal dec_shift_lsr_inter: Std_Logic;
signal dec_shift_asr_inter: Std_Logic;
signal dec_shift_ror_inter: Std_Logic;
signal dec_shift_rrx_inter: Std_Logic;
signal dec_shift_val_inter: Std_Logic_Vector(4 downto 0);
signal dec_cy_inter: Std_Logic;



component reg_vide
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
		reg_rd1		: out Std_Logic_Vector(31 downto 0); --input plustot
		radr1			: in Std_Logic_Vector(3 downto 0);
		reg_v1		: out Std_Logic;

	-- Read Port 2 32 bits
		reg_rd2		: out Std_Logic_Vector(31 downto 0);
		radr2			: in Std_Logic_Vector(3 downto 0);
		reg_v2		: out Std_Logic;

	-- Read Port 3 5 bits (for shift)
		reg_rd3		: out Std_Logic_Vector(31 downto 0);
		radr3			: in Std_Logic_Vector(3 downto 0);
		reg_v3		: out Std_Logic;

	-- read CSPR Port
		reg_cry			: out Std_Logic;
		reg_zero		: out Std_Logic;
		reg_neg			: out Std_Logic;
		reg_ovr			: out Std_Logic;
		
		reg_cznv		: out Std_Logic;
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
		ck					: in Std_Logic;
		reset_n			: in Std_Logic;
		vdd				: in bit;
		vss				: in bit);
end component;

signal cond	: Std_Logic;
signal cond_en	: Std_Logic;

signal regop_t  : Std_Logic;
signal mult_t   : Std_Logic;
signal swap_t   : Std_Logic;
signal trans_t  : Std_Logic;
signal mtrans_t : Std_Logic;
signal branch_t : Std_Logic;

-- regop instructions
signal and_i  : Std_Logic;
signal eor_i  : Std_Logic;
signal sub_i  : Std_Logic;
signal rsb_i  : Std_Logic;
signal add_i  : Std_Logic;
signal adc_i  : Std_Logic;
signal sbc_i  : Std_Logic;
signal rsc_i  : Std_Logic;
signal tst_i  : Std_Logic;
signal teq_i  : Std_Logic;
signal cmp_i  : Std_Logic;
signal cmn_i  : Std_Logic;
signal orr_i  : Std_Logic;
signal mov_i  : Std_Logic;
signal bic_i  : Std_Logic;
signal mvn_i  : Std_Logic;

-- mult instruction
signal mul_i  : Std_Logic;
signal mla_i  : Std_Logic;

-- trans instruction
signal ldr_i  : Std_Logic;
signal str_i  : Std_Logic;
signal ldrb_i : Std_Logic;
signal strb_i : Std_Logic;

-- mtrans instruction
signal ldm_i  : Std_Logic;
signal stm_i  : Std_Logic;

-- branch instruction
signal b_i    : Std_Logic;
signal bl_i   : Std_Logic;

-- Multiple transferts

-- RF read ports

-- Flags
signal cry	: Std_Logic;
signal zero	: Std_Logic;
signal neg	: Std_Logic;
signal ovr	: Std_Logic;


-- DECOD FSM


begin

	dec2exec : fifo
	--generic map (WIDTH => ???)
	port map (	din() => pre_index,
-- Execution condition

	cond <= '1' when	(if_ir(31 downto 28) = X"0" and zero = '1') or
				(if_ir(31 downto 28) = X"1" and zero = '0') or
				(if_ir(31 downto 28) = X"2" and cry = '1') or
				(if_ir(31 downto 28) = X"3" and cry = '0') or
				(if_ir(31 downto 28) = X"4" and neg = '1') or
				(if_ir(31 downto 28) = X"5" and neg = '0') or
				(if_ir(31 downto 28) = X"6" and ovr = '1') or
				(if_ir(31 downto 28) = X"7" and ovr = '0') or
				(if_ir(31 downto 28) = X"8" and cry = '1' and zero = '0') or
				(if_ir(31 downto 28) = X"9" and cry = '0' or zero = '1') or
				(if_ir(31 downto 28) = X"A" and neg=ovr) or
				(if_ir(31 downto 28) = X"B" and neg/=ovr) or
				(if_ir(31 downto 28) = X"C" and zero= '0' and (neg=ovr)) or
				(if_ir(31 downto 28) = X"D" and zero='0' or (neg/=ovr)) or
				(if_ir(31 downto 28) = X"E") else '0';

	condv <= '1'		when if_ir(31 downto 28) = X"E" else
		reg_cznv	when if_ir(31 downto 28) = X"0" or
				 if_ir(31 downto 28) = X"1" or
				 if_ir(31 downto 28) = X"2" or
				 if_ir(31 downto 28) = X"3" or
				 if_ir(31 downto 28) = X"4" or
				 if_ir(31 downto 28) = X"5" 
		reg_vv		when if_ir(31 downto 28) = X"6" or
				 if_ir(31 downto 28) = X"7"
		

		reg_czn and reg_vv
				when if_ir(31 downto 28) = X"8" or
				if_ir(31 downto 28) = X"9" or
				if_ir(31 downto 28) = X"A" or
				if_ir(31 downto 28) = X"B" or
				if_ir(31 downto 28) = X"C" or
				if_ir(31 downto 28) = X"D"; 

-- decod instruction type

	regop_t <= '1' when if_ir(27 downto 26) = "00" else '0';
	mult_t <= '1' when if_ir(27 downto 22) = "000000" and if_ir(7 downto 4) = "1001" else '0';
	swap_t <= '1' when if_ir(27 downto 23) = "00010" and if_ir(21 downto 20) = "00" and if_ir(11 downto 4) = "00001001" else '0';
	trans_t <= '1' when if_ir(27 downto 26) = "01" else '0';--single data transfer
	mtrans_t <= '1' when if_ir(27 downto 25) = "100" else '0';--block data transfer
	branch_t <= '1' when if_ir(27 downto 25) = "101" else '0';


-- decod regop opcode

	and_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"0" else '0';
	ero_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"1" else '0';
	sub_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"2" else '0';
	rsb_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"3" else '0';
	add_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"4" else '0';
	adc_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"5" else '0';
	sbc_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"6" else '0';
	rsc_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"7" else '0';
	tst_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"8" else '0';
	teq_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"9" else '0';
	cmp_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"A" else '0';
	cmn_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"B" else '0';
	orr_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"C" else '0';
	mov_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"D" else '0';
	bic_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"E" else '0';
	mvn_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"F" else '0';

-- decod mult_t opcode

	mul_i <= '1' when mult_t = '1' and if_ir(21) = '0' else '0';
	mla_i <= '1' when mult_t = '1' and if_ir(21) = '1' else '0';

-- decod trans_t opcode

	ldr_i <= '1' when trans_t = '1' and if_ir(20) = '1' and if_ir(22) = '0' else '0';
	str_i <= '1' when trans_t = '1' and if_ir(20) = '0' and if_ir(22) = '0' else '0';
	ldrb_i <= '1' when trans_t = '1' and if_ir(20) = '1' and if_ir(22) = '1' else '0';
	strb_i <= '1' when trans_t = '1' and if_ir(20) = '0' and if_ir(22) = '1' else '0';

-- decod mtrans_t opcode

	ldm_i <= '1' when mtrans_t = '1' and if_ir(20) = '1';
	stem_i <= '1' when mtrans_t = '1' and if_ir(20) = '0';

-- decod branch_t opcode
	
	b_i <= '1' when branch_t = '1' and if_ir(24) = '0';
	bl_i <= '1' when branch_t = '1' and if_ir(24) = '1';

-- data processing Immediate
	
	imm_regop <= if_ir(25) when regop = '1';

-- single data transfer immediate

	imm_trans <= if_ir(25) when trans_t = '1';
	
--
variable Rot_val(3 downto 0);
signal Rot_val_signal (4 downto 0);
-- Operand
	Rn_n <= if_ir(19 downto 16) when regop = '1' or swap_t = '1' or trans_t = '1' or mtrans_t = '1' else if_ir(15 downto 12) when mult_t = '1';
	Rd_n <= if_ir(15 downto 12) when regop = '1' or swap_t = '1' or trans_t = '1' else if_ir(19 downto 16) when mult_t = '1';
	Rs_n <= if_ir(11 downto 8) when mult_t = '1';
	Rm_n <= if_ir(3 downto 0) when imm_regop = '0' or mult_t = '1' or imm_trans = '0';
	Rot_val :=to_integer(signed(if_ir(11 downto 8)));
	Rot_val :=Rot_val*2;
	Rot_val_signal <= std_logic_vector(to_unsigned(Rot_val,5));
	
	
-- Read operand
	
signal op1: std_logic_vector(31 downto 0);
signal op1_v: std_logic;
signal op2: std_logic_vector(31 downto 0);
signal op2_v: std_logic;
signal op3: std_logic_vector(31 downto 0);
signal op3_v: std_logic;

-- Read cspr
signal cry: std_logic;
signal zero: std_logic;
signal neg: std_logic;
signal ovr: std_logic;
signal cznv: std_logic;
signal vv: std_logic;


-- operand 2 shift type
	dec_shift_lsl_inter	<= '1' when if_ir(6 downto 5) = "00" else '0';
	dec_shift_lsr_inter	<= '1' when if_ir(6 downto 5) = "01" else '0';
	dec_shift_asr_inter	<= '1' when if_ir(6 downto 5) = "10" else '0';
	dec_shift_ror_inter	<= '1' when if_ir(6 downto 5) = "11" and Rs/=x"00000000" else '0';
	dec_shift_rrx_inter	<= '1' when if_ir(6 downto 5) = "11" and Rs =x"00000000" else '0';
	dec_shift_val_inter	<= Rs when if_ir(7) = '0' and if_ir(4) = '1' else if_ir(11 downto 7) when if_ir(4) = '0' else "0000";
	dec_cy_inter		<= dec_cy <= '1' when (rsb_i or rsc_i or sub_i or sbc_i or cmp_i or bic_i) = '1' else '0';
	

-- ALU operand
	dec_comp_op1 <= Rn when regop = '1';
	dec_comp_op2 <= Rm when imm_regop = '0' else imm when imm_regop = '1' ;

-- Signal pour controle l'invalidite
signal inval_adr_dec: std_logic_vector(3 downto 0);
signal inval1_dec:std_logic;
signal inval_adr_dec:std_logic_vector(3 downto 0);
signal inval2_dec:std_logic;
signal inval_czn_dec: std_logic_vector;
signal inval_ovr_dec: Std_Logic;

-- Signal pc
signal pc_dec: std_logic_vector(31 downto 0);
signal pc_v_dec: std_logic;
signal pc_inc_dec: std_logic;
signal dec_if_push: std_logic;


-- signal of fifo decod to ifecth
signal fifo_32_empty: std_logic;
signal fifo_32_full: std_logic;
signal dec_if_full: std_logic;
signal dec_if_empty: std_logic;


-- signal de load
signal dec_mem_lw_inter: std_logic;
signal dec_mem_lb_inter: std_logic;
signal dec_mem_sw_inter: std_logic;
signal dec_mem_sb_inter: std_logic;

-- signal of mae
signal T1,T2,T3,T4,T5,T6: std_logic;





-- REG component

exec_reg : reg_vide port map(
	-- Write Port 1 prioritaire
		wdata1 => exe_res;
		wadr1 => exe_dest;
		wen1 => exe_wb;

	-- Write Port 2 non prioritaire
		wdata2 => mem_res;
		wadr2 => mem_dest;
		wen2 => mem_wb;

	-- Write CSPR Port
		wcry => exe_c;
		wzero => exe_z;
		wneg => exe_n;
		wovr => exe_v;
		cspr_wb => exe_flag_wb;
		
	-- Read Port 1 32 bits
		reg_rd1 => op1;
		radr1 => Rn_n;
		reg_v1 => op1_v;

	-- Read Port 2 32 bits
		reg_rd2 => op2;
		radr2 => Rs_n;
		reg_v2 => op2_v;

	-- Read Port 3 5 bits (for shift)
		reg_rd3 => op3;
		radr3 => Rm_n;
		reg_v3 => op3_v;

	-- read CSPR Port
		reg_cry => cry ;
		reg_zero => zero;
		reg_neg => neg;
		reg_ovr => ovr;
		
		reg_cznv => cznv;
		reg_vv => vv;

	-- Invalidate Port 
		inval_adr1 => inval_adr_dec;
		inval1	=> inval1_dec;

		inval_adr2 => inval_adr_dec;
		inval2	=> inval2_dec;

		inval_czn => inval_czn_dec;
		inval_ovr => inval_ovr_dec;

	-- PC
		reg_pc => pc_dec;
		reg_pcv	=> pc_v_dec;
		inc_pc	=> pc_inc_dec;
	
	-- global interface
		ck => ck;
		reset_n	=> reset_n;
		vdd => vdd;
		vss => vss);


dec2if_fifo : fifo_32b port map(
		din => pc_dec,
		dout => dec_pc,
		-- commands
		push => dec_if_push,
		pop => if_pop,
		-- flags
		full => dec_if_full,
		empty => dec_if_empty,
		
		reset_n => reset_n,
		ck => ck,
		vdd => vdd,
		vss => vss );


type state_type is (FETCH,RUN,LINK,BRANCH,MTRANS)
signal cur_state,next_state:state_type;

T1 <= not(dec_if_empty) and if2dec_empty;
T2 <= not(if2dec_empty);


process(ck)
begin
	if(reset_n='0') then etat_present <= FETCH;
	elsif (rising_edge(ck)) then
		case etat_present is
		when FETCH => etat_present <= FETCH;
			if T2 = '1' then etat_present <= RUN;
			end if;
		when RUN => etat_present <= RUN;
			if T4 = '1' then etat_present <= LINK;
			elsif T5 = '1' then etat_present <= BRANCH;
			elsif T6 = '1' then etat_present <= MTRANS;
			end if;
		when LINK => etat_present <= LINK;
			if T1 = '1' then etat_present <= BRANCH;
			end if;
		when BRANCH => etat_present <= BRANCH;
			if T2 = '1' then etat_present <= RUN;
			elsif T3 = '1' then etat_present <= FETCH;
			end if;
		when MTRANS => etat_present <= MTRANS;
			if T2 = '1' then etat_present <= RUN;
			elsif T3 = '1' then etat_present <= BRANCH;
			end if;
		when others => etat_present <= FETCH;
		end case;
	end if;
end process;



------ Signaux des commandes de shifter -------


dec_shift_lsl <= dec_shift_lsl_inter;
dec_shift_lsr <= dec_shift_lsr_inter;
dec_shift_asr <= dec_shift_asr_inter;
dec_shift_ror <= dec_shift_ror_inter;
dec_shift_rrx <= dec_shift_rrx_inter;
dec_shift_val <= dec_shift_val_inter;
dec_cy <= dec_cy_inter;




------ Signaux des loads/saves -------



dec_mem_lw <= ldr_i;
dec_mem_sw <= str_i;
dec_mem_lb <= ldrb_i;
dec_mem_sb <= strb_i;






------ Signaux de ALU -------



dec_alu_cmd <= "00" when (sub_i or rsb_i or add_i or adc_i or sbc_i or rsc_i or cmp_i or cmn_i or mov_i or mvn_i) = '1'
				else "01" when (and_i or tst_i or bic_i) = '1'
				else "10" when orr_i = '1'
				else "11" when (eor_i or teq_i) = '1';

dec_alu_cy <= '1' when (sub_i or rsb_i or sbc_i or rsc_i or cmp_i) = '1' else '0' ;





------ Signaux de comparateur -------


dec_comp_op1 <= '1' when (rsb_i or rsc_i) = '1' else '0';

dec_comp_op2 <= '1' when (sub_i or sbc_i or cmp_i or bic_i or mvn_i) = '1' else '0' ; 





------ Signaux de pc incrementation -------



pc_inc_dec <= '0' when etat_present = BRANCH else dec_if_push;






dec_if_push <= '0' when dec_if_full = '1' else pc_v_dec;
dec_pop <= '1' when etat_present = RUN else '0';
dec2exe_empty <= '1' when etat_present = RUN else '0';
dec2if_empty <= dec_if_empty;

dec_op1 <= op1;
dec_op2 <= op2;
dec_exe_dest <= Rd_n;

inval_adr_dec <= Rd_n;
inval1_dec <= exe_wb;

dec_flag_wb <= if_ir(20);

inval_czn_dec <= if_ir(20);
inval_ovr_dec <= if_ir(20);

dec_exe_wb <= if_ir(21);


end Behavior;
