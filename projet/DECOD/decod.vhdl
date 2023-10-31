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
			dec_alu_cmd			: out std_logic_vector(1 downto 0);

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

--signal Rn : std_logic_vector(3 downto 0);
--signal Rd : std_logic_vector(3 downto 0);
--signal Rs : std_logic_vector(3 downto 0);
--signal Rm : std_logic_vector(3 downto 0);
--signal operand2 : std_logic_vector(11 downto 0);

signal dec_shift_lsl_inter: Std_Logic;
signal dec_shift_lsr_inter: Std_Logic;
signal dec_shift_asr_inter: Std_Logic;
signal dec_shift_ror_inter: Std_Logic;
signal dec_shift_rrx_inter: Std_Logic;
signal dec_shift_val_inter: Std_Logic_Vector(4 downto 0);
signal dec_cy_inter: Std_Logic;

component fifo_127b
	port(
		din		: in std_logic_vector(126 downto 0);
		dout		: out std_logic_vector(126 downto 0);

		-- commands
		push		: in std_logic;
		pop		: in std_logic;

		-- flags
		full		: out std_logic;
		empty		: out std_logic;

		reset_n	: in std_logic;
		ck			: in std_logic;
		vdd		: in bit;
		vss		: in bit);
end Component;

component fifo_32b
	port(
		din		: in std_logic_vector(31 downto 0);
		dout		: out std_logic_vector(31 downto 0);

		-- commands
		push		: in std_logic;
		pop		: in std_logic;

		-- flags
		full		: out std_logic;
		empty		: out std_logic;

		reset_n	: in std_logic;
		ck			: in std_logic;
		vdd		: in bit;
		vss		: in bit);
end Component;


component reg
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
signal condv	: Std_Logic;

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


signal imm_regop: std_logic;
signal imm_trans: std_logic;


signal Rn_n: std_logic_vector(3 downto 0);
signal Rd_n: std_logic_vector(3 downto 0);
signal Rs_n: std_logic_vector(3 downto 0);
signal Rm_n: std_logic_vector(3 downto 0);

signal read_port1: std_logic_vector(3 downto 0);


signal Rot_val_signal: std_logic_vector(4 downto 0);

-- DECOD FSM



-- Signal pour controle l'invalidite
signal inval1_adr_dec: std_logic_vector(3 downto 0);
signal inval1_dec:std_logic;
signal inval2_adr_dec:std_logic_vector(3 downto 0);
signal inval2_dec:std_logic;
signal inval_czn_dec: std_logic;
signal inval_ovr_dec: Std_Logic;

-- Signal pc
signal pc_dec: std_logic_vector(31 downto 0);
signal pc_v_dec: std_logic;
signal pc_inc_dec: std_logic;
signal dec_if_push: std_logic;


-- signal of fifo decod to ifecth
signal dec_if_full: std_logic;
signal dec_if_empty: std_logic;


-- signal de load
signal dec_mem_lw_inter: std_logic;
signal dec_mem_lb_inter: std_logic;
signal dec_mem_sw_inter: std_logic;
signal dec_mem_sb_inter: std_logic;



signal dec_exe_wb_sig: std_logic;
signal dec_flag_wb_sig: std_logic;
signal dec_pre_index_sig: std_logic;

signal dec_comp_op1_sig :std_logic;
signal dec_comp_op2_sig :std_logic;

signal dec_alu_cy_sig :std_logic;
signal dec_alu_cmd_sig :std_logic_vector(1 downto 0);

signal dec_exe_full:std_logic;
signal dec_exe_empty:std_logic;

signal dec_exe_push:std_logic;


-- Read operand
	
signal op1: std_logic_vector(31 downto 0);
signal op1_v: std_logic;
signal op2: std_logic_vector(31 downto 0);
signal op2_v: std_logic;
signal op3: std_logic_vector(31 downto 0);
signal op3_v: std_logic;

signal op1_sig: std_logic_vector(31 downto 0);
signal op2_sig: std_logic_vector(31 downto 0);
signal op3_sig: std_logic_vector(31 downto 0);

-- Read cspr
signal cry: std_logic;
signal zero: std_logic;
signal neg: std_logic;
signal ovr: std_logic;
signal cznv: std_logic;
signal vv: std_logic;

-- Signal MAE
type state_type is (FETCH,RUN,LINK,BRANCH,MTRANS);
signal cur_state,next_state:state_type;

signal T1_FETCH,T2_FETCH,T1_RUN,T2_RUN,T3_RUN,T4_RUN,T5_RUN,T6_RUN,T1_LINK,T1_BRANCH,T2_BRANCH,T3_BRANCH,T1_MTRANS,T2_MTRANS: std_logic;


begin

	--dec2exec : fifo
	--generic map (WIDTH => ???)
	--port map (	din() => pre_index,
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
				(if_ir(31 downto 28) = X"9" and (cry = '0' or zero = '1')) or
				(if_ir(31 downto 28) = X"A" and neg=ovr) or
				(if_ir(31 downto 28) = X"B" and neg/=ovr) or
				(if_ir(31 downto 28) = X"C" and zero= '0' and (neg=ovr)) or
				(if_ir(31 downto 28) = X"D" and (zero='0' or (neg/=ovr))) or
				(if_ir(31 downto 28) = X"E") else '0';

	condv <= '1'		when if_ir(31 downto 28) = X"E" else
		cznv	when if_ir(31 downto 28) = X"0" or
				 if_ir(31 downto 28) = X"1" or
				 if_ir(31 downto 28) = X"2" or
				 if_ir(31 downto 28) = X"3" or
				 if_ir(31 downto 28) = X"4" or
				 if_ir(31 downto 28) = X"5" 
		else vv		when if_ir(31 downto 28) = X"6" or
				 if_ir(31 downto 28) = X"7"
		

		else cznv and vv
				when if_ir(31 downto 28) = X"8" or
				if_ir(31 downto 28) = X"9" or
				if_ir(31 downto 28) = X"A" or
				if_ir(31 downto 28) = X"B" or
				if_ir(31 downto 28) = X"C" or
				if_ir(31 downto 28) = X"D"; 

-- decod instruction type

	regop_t <= '1' when condv <= '1' and if_ir(27 downto 26) = "00" else '0';
	mult_t <= '1' when condv <= '1' and if_ir(27 downto 22) = "000000" and if_ir(7 downto 4) = "1001" else '0';
	swap_t <= '1' when condv <= '1' and if_ir(27 downto 23) = "00010" and if_ir(21 downto 20) = "00" and if_ir(11 downto 4) = "00001001" else '0';
	trans_t <= '1' when condv <= '1' and if_ir(27 downto 26) = "01" else '0';--single data transfer
	mtrans_t <= '1' when condv <= '1' and if_ir(27 downto 25) = "100" else '0';--block data transfer
	branch_t <= '1' when condv <= '1' and if_ir(27 downto 25) = "101" else '0';


-- decod regop opcode

	and_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"0" else '0';
	eor_i <= '1' when regop_t = '1' and if_ir(24 downto 21) = X"1" else '0';
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
	stm_i <= '1' when mtrans_t = '1' and if_ir(20) = '0';

-- decod branch_t opcode
	
	b_i <= '1' when branch_t = '1' and if_ir(24) = '0';
	bl_i <= '1' when branch_t = '1' and if_ir(24) = '1';

-- data processing Immediate
	
	imm_regop <= if_ir(25) when regop_t = '1';

-- single data transfer immediate

	imm_trans <= if_ir(25) when trans_t = '1';
	
--

-- Operand
	Rn_n <= if_ir(19 downto 16) when regop_t = '1' or swap_t = '1' or trans_t = '1' or mtrans_t = '1' else if_ir(15 downto 12) when mult_t = '1';
	Rd_n <= if_ir(15 downto 12) when regop_t = '1' or swap_t = '1' or trans_t = '1' else if_ir(19 downto 16) when mult_t = '1';
	Rs_n <= if_ir(11 downto 8);
	Rm_n <= if_ir(3 downto 0) when imm_regop = '0' or mult_t = '1' or imm_trans = '0';

	Rot_val_signal <= if_ir(11 downto 8) & '0';
	
	read_port1 <= Rn_n when regop_t = '1' else Rd_n when (str_i or strb_i) = '1';




-- operand 2 shift type
	dec_shift_lsl_inter	<= '1' when if_ir(6 downto 5) = "00" else '0';
	dec_shift_lsr_inter	<= '1' when if_ir(6 downto 5) = "01" else '0';
	dec_shift_asr_inter	<= '1' when if_ir(6 downto 5) = "10" else '0';
	dec_shift_ror_inter	<= '1' when if_ir(6 downto 5) = "11" and Rs_n/=x"00000000" else '0';
	dec_shift_rrx_inter	<= '1' when (if_ir(6 downto 5) = "11" and Rs_n =x"00000000") or imm_regop = '1' else '0';
	dec_shift_val_inter	<= if_ir(11 downto 7) when (if_ir(4)='0' and imm_regop = '0') else op3_sig(4 downto 0) when (if_ir(4) = '1' and imm_regop = '0') else Rot_val_signal when imm_regop = '1' else "00000";
	dec_cy_inter		<=  '1' when (rsb_i or rsc_i or sub_i or sbc_i or cmp_i or bic_i) = '1' else '0';
	

-- ALU operand
	--dec_comp_op1 <= Rn_n when regop_t = '1';
	--dec_comp_op2 <= Rm_n when imm_regop = '0' else imm when imm_regop = '1' ;

op1_sig <= x"00000000" when (mov_i or mvn_i) = '1' else op1;
op2_sig <= x"000000" & if_ir(7 downto 0) when imm_regop ='1' else op2;
op3_sig <= x"00000000" when imm_regop = '0' else op3;


dec_pre_index_sig <= if_ir(24) when trans_t='1'else '0';

fifo_127b_i : fifo_127b
	port map (
					din(126 downto 95)		=> op1,
					din(94 downto 63)		=> op2_sig,
					din(62 downto 59)		=> Rd_n,
					din(58)				=> dec_exe_wb_sig,
					din(57)				=> dec_flag_wb_sig,
					din(56 downto 25)		=> op1,
					din(24 downto 21)		=> Rd_n,
					din(20)				=> dec_pre_index_sig,--dec_pre_index
					din(19)				=> ldr_i,
					din(18)				=> ldrb_i,
					din(17)				=> str_i,
					din(16)				=> strb_i,
					din(15)				=> dec_shift_lsl_inter,--dec_shift_lsl
					din(14)				=> dec_shift_lsr_inter,
					din(13)				=> dec_shift_asr_inter,
					din(12)				=> dec_shift_ror_inter,
					din(11)				=> dec_shift_rrx_inter,
					din(10 downto 6)		=> dec_shift_val_inter,--dec_shift_val
					din(5)				=> dec_cy_inter,
					din(4)				=> dec_comp_op1_sig,
					din(3)				=> dec_comp_op2_sig,
					din(2)				=> dec_alu_cy_sig,
					din(1 downto 0)			=> dec_alu_cmd_sig,

					dout(126 downto 95)		=> dec_op1,
					dout(94 downto 63)		=> dec_op2,
					dout(62 downto 59)		=> dec_exe_dest,
					dout(58)			=> dec_exe_wb,
					dout(57)			=> dec_flag_wb,
					dout(56 downto 25)		=> dec_mem_data,
					dout(24 downto 21)		=> dec_mem_dest,
					dout(20)			=> dec_pre_index,
					dout(19)			=> dec_mem_lw,
					dout(18)			=> dec_mem_lb,
					dout(17)			=> dec_mem_sw,
					dout(16)			=> dec_mem_sb,
					dout(15)			=> dec_shift_lsl,
					dout(14)			=> dec_shift_lsr,
					dout(13)			=> dec_shift_asr,
					dout(12)			=> dec_shift_ror,
					dout(11)			=> dec_shift_rrx,
					dout(10 downto 6)		=> dec_shift_val,
					dout(5)				=> dec_cy,
					dout(4)				=> dec_comp_op1,
					dout(3)				=> dec_comp_op2,
					dout(2)				=> dec_alu_cy,
					dout(1 downto 0)		=> dec_alu_cmd,
					-- commands
					push		=> dec_exe_push,
					pop		=> exe_pop,

					-- flags
					full		=> dec_exe_full,
					empty		=> dec_exe_empty,

					reset_n			=> reset_n,
					ck		 			=> ck,
					vdd	 			=> vdd,
					vss	 			=> vss);



-- REG component

exec_reg : reg port map(
	-- Write Port 1 prioritaire
		wdata1 => exe_res,
		wadr1 => exe_dest,
		wen1 => exe_wb,

	-- Write Port 2 non prioritaire
		wdata2 => mem_res,
		wadr2 => mem_dest,
		wen2 => mem_wb,

	-- Write CSPR Port
		wcry => exe_c,
		wzero => exe_z,
		wneg => exe_n,
		wovr => exe_v,
		cspr_wb => exe_flag_wb,
		
	-- Read Port 1 32 bits
		reg_rd1 => op1,
		radr1 => read_port1,
		reg_v1 => op1_v,

	-- Read Port 2 32 bits
		reg_rd2 => op2,
		radr2 => Rm_n,
		reg_v2 => op2_v,

	-- Read Port 3 5 bits (for shift)
		reg_rd3 => op3,
		radr3 => Rs_n,
		reg_v3 => op3_v,

	-- read CSPR Port
		reg_cry => cry ,
		reg_zero => zero,
		reg_neg => neg,
		reg_ovr => ovr,
		
		reg_cznv => cznv,
		reg_vv => vv,

	-- Invalidate Port 
		inval_adr1 => inval1_adr_dec,
		inval1	=> inval1_dec,

		inval_adr2 => inval2_adr_dec,
		inval2	=> inval2_dec,

		inval_czn => inval_czn_dec,
		inval_ovr => inval_ovr_dec,

	-- PC
		reg_pc => pc_dec,
		reg_pcv	=> pc_v_dec,
		inc_pc	=> pc_inc_dec,
	
	-- global interface
		ck => ck,
		reset_n	=> reset_n,
		vdd => vdd,
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







T1_FETCH <= not(dec_if_empty) and if2dec_empty;
T2_FETCH <= dec_if_empty;
T1_RUN   <= if2dec_empty or not(condv);
T2_RUN   <= not(T1_RUN) and not(cond);
T3_RUN   <= not(T2_RUN);

process(ck)
begin
if(rising_edge(ck)) then
	if(reset_n = '0') then
		cur_state <= FETCH;
	else
		cur_state <= next_state;
	end if;
end if;
end process;


next_state <= FETCH when (cur_state = FETCH and T1_FETCH = '1')
		    or	 (cur_state = BRANCH and T3_BRANCH ='1')
	else
	      RUN	when (cur_state = FETCH and T2_FETCH = '1')
			or   (cur_state = RUN and (T1_RUN or T2_RUN or T3_RUN)='1')
			or   (cur_state = MTRANS and T2_MTRANS = '1')
			or   (cur_state = BRANCH and T2_BRANCH = '1')
	else
	      LINK	when (cur_state = RUN and T4_RUN = '1')
	else
	      BRANCH	when (cur_state = RUN and T5_RUN = '1')
			or   (cur_state = BRANCH and T1_BRANCH = '1')
			or   (cur_state = LINK and T1_LINK = '1')
	else MTRANS	when (cur_state = RUN and T6_RUN = '1')
			or   (cur_state = RUN and T1_MTRANS = '1')
	else
			FETCH;



------ Signaux des commandes de shifter -------


--dec_shift_lsl <= dec_shift_lsl_inter;
--dec_shift_lsr <= dec_shift_lsr_inter;
--dec_shift_asr <= dec_shift_asr_inter;
--dec_shift_ror <= dec_shift_ror_inter;
--dec_shift_rrx <= dec_shift_rrx_inter;
--dec_shift_val <= dec_shift_val_inter;
--dec_cy <= dec_cy_inter;




------ Signaux des loads/saves -------



--dec_mem_lw <= ldr_i;
--dec_mem_sw <= str_i;
--dec_mem_lb <= ldrb_i;
--dec_mem_sb <= strb_i;






------ Signaux de ALU -------



dec_alu_cmd_sig <= "00" when (sub_i or rsb_i or add_i or adc_i or sbc_i or rsc_i or cmp_i or cmn_i or mov_i or mvn_i) = '1'
				else "01" when (and_i or tst_i or bic_i) = '1'
				else "10" when orr_i = '1'
				else "11" when (eor_i or teq_i) = '1';

dec_alu_cy_sig <= '1' when (sub_i or rsb_i or sbc_i or rsc_i or cmp_i) = '1' else '0' ;





------ Signaux de comparateur -------


dec_comp_op1_sig <= '1' when (rsb_i or rsc_i) = '1' else '0';

dec_comp_op2_sig <= '1' when (sub_i or sbc_i or cmp_i or bic_i or mvn_i) = '1' else '0' ; 





------ Signaux de pc incrementation -------



pc_inc_dec <= '0' when cur_state = BRANCH else dec_if_push;







dec_pop <= '0' when (cur_state = RUN and T1_RUN = '1' and if2dec_empty = '1')  else '1' when ((cur_state = RUN) and ((T3_RUN or T2_RUN) = '1') and (T1_RUN = '0'));

--dec2exe_empty <= '1' when cur_state = RUN else '0';

dec2if_empty <= dec_if_empty;
dec2exe_empty <= dec_exe_empty;

--dec_op1 <= op1;
--dec_op2 <= op2;
--dec_exe_dest <= Rd_n;

inval1_adr_dec <= Rd_n;
inval1_dec <= exe_wb;

dec_flag_wb_sig <= if_ir(20);

inval_czn_dec <= if_ir(20);
inval_ovr_dec <= if_ir(20);

dec_exe_wb_sig <= '1' when (and_i or eor_i or sub_i or rsb_i or add_i or adc_i or sbc_i or rsc_i or orr_i or mov_i or bic_i or mvn_i)='1';

dec_exe_push <= '0' when (cur_state = FETCH and T1_FETCH = '1') or (cur_state = RUN and (T1_RUN or T2_RUN)='1')
		else '1' when (cur_state = RUN and T3_RUN = '1');

dec_if_push <= '1' when (cur_state = FETCH and T2_FETCH = '1') or
					(cur_state = RUN and (T1_RUN or T2_RUN or T3_RUN)='1' and dec_if_empty='1')
				else '0' when (cur_state = FETCH and T1_FETCH = '1') or 
					(cur_state = RUN and (T1_RUN or T2_RUN or T3_RUN)='1' and (dec_if_empty)='0');




end Behavior;
