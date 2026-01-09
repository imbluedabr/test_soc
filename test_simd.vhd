
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ALU is

    generic (
        N : integer := 8
    );
    
    port (
        operand_a : in std_logic_vector(N-1 downto 0);
        operand_b : in std_logic_vector(N-1 downto 0);
        mode : in std_logic_vector(1 downto 0);
        result : out std_logic_vector(N-1 downto 0);
        flags_in : in std_logic_vector(1 downto 0); -- 0: Z, 1: C
        flags_out : out std_logic_vector(1 downto 0);
        enable : in std_logic
    );

end entity ALU;

architecture ALU_impl of ALU is
    signal add_res : unsigned(N downto 0);
    signal b_unsigned : unsigned(N downto 0);
    signal carry_in : unsigned(0 downto 0);
begin
    --modes: 0 - ADD, 1 - ADDC, 2 - SUB, 3 - SUBC
    b_unsigned <= ('0' & unsigned(not operand_b)) when mode(1) = '1' else
                  ('0' & unsigned(operand_b));

    carry_in <= "1" when mode(0) = '1' and flags_in(1) = '1' else
                "1" when mode = "10" else
                "0";

    add_res <= ('0' & unsigned(operand_a)) + b_unsigned + carry_in;

    result <= std_logic_vector(add_res(N-1 downto 0)) when enable = '1' else
              operand_b;
    
    flags_out(0) <= '1' when add_res(N-1 downto 0) = to_unsigned(0, N) else
                    '0';
    flags_out(1) <= add_res(N);

end architecture ALU_impl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity SIMD_core is
    generic (
        N : integer := 8
    );

    port (
        data_in : in std_logic_vector(N-1 downto 0);
        data_out : out std_logic_vector(N-1 downto 0);
        data_adres : out std_logic_vector((2*N)-1 downto 0);
        program_in : in std_logic_vector(N+7 downto 0);
        program_adres : out std_logic_vector(N-1 downto 0);
        bus_req : out std_logic;
        we : out std_logic;
        reset : in std_logic;
        clk : in std_logic;
		debug_port : out std_logic_vector(N-1 downto 0)
    );

end entity SIMD_core;

architecture SIMD_core_impl of SIMD_core is
    
    component ALU is

        generic (
            N : integer := 8
        );
        
        port (
            operand_a : in std_logic_vector(N-1 downto 0);
            operand_b : in std_logic_vector(N-1 downto 0);
            mode : in std_logic_vector(1 downto 0);
            result : out std_logic_vector(N-1 downto 0);
            flags_in : in std_logic_vector(1 downto 0);
            flags_out : out std_logic_vector(1 downto 0);
            enable : in std_logic
        );

    end component ALU;

    --register control
    signal r1, r2, r3, r4 : std_logic_vector(N-1 downto 0) := (others => '0');
    signal reg_port0 : std_logic_vector(N-1 downto 0);
    signal reg_port1 : std_logic_vector(N-1 downto 0);
    signal reg_select0 : std_logic_vector(1 downto 0);
    signal reg_select1 : std_logic_vector(1 downto 0);
    signal reg_input : std_logic_vector(N-1 downto 0);
    
    --alu control
    signal alu0_mode : std_logic_vector(1 downto 0);
    signal alu0_result : std_logic_vector(N-1 downto 0);
    signal alu0_flags : std_logic_vector(1 downto 0);
    signal alu0_newflags : std_logic_vector(1 downto 0);
    signal alu0_inputb : std_logic_vector(N-1 downto 0);

    --cpu control
    signal instruction_register : std_logic_vector(N+7 downto 0) := (others => '0');
    signal instruction_pointer : unsigned(N-1 downto 0) := (others => '0');
    signal operand : std_logic_vector(N-1 downto 0);
    signal opcode : std_logic_vector(7 downto 0);
    
    type control_t is record
        set_reg : std_logic;
        en_alu : std_logic;
        sel_reg_inp : std_logic;
        sel_alu_inp : std_logic_vector(1 downto 0);
        alu_mode : std_logic_vector(1 downto 0);
        busrq : std_logic;
        write : std_logic;
        set_ip : std_logic;
    end record;
    signal control_signals : control_t;
    type cycle_state is (FE, EX, WB);
    signal cpu_state : cycle_state := WB;
	attribute preserve : boolean; -- preserve the signal
	attribute preserve of cpu_state : signal is true;
	attribute keep : boolean;
	attribute keep of bus_req : signal is true;
begin
    
    --register file
    reg_port1 <= r1 when reg_select1 = "00" else
                 r2 when reg_select1 = "01" else
                 r3 when reg_select1 = "10" else
                 r4 when reg_select1 = "11" else
                 (others => '0');
    reg_port0 <= r1 when reg_select0 = "00" else
                 r2 when reg_select0 = "01" else
                 r3 when reg_select0 = "10" else
                 r4 when reg_select0 = "11" else
                 (others => '0');
    
	debug_port <= reg_input;
	
    reg_select0 <= opcode(5 downto 4);
    reg_select1 <= opcode(7 downto 6);

    reg_input <= data_in when control_signals.sel_reg_inp = '0' else
                 alu0_result when control_signals.sel_reg_inp = '1' else
                 (others => '0');

    --alu control
    with control_signals.sel_alu_inp select
        alu0_inputb <=  reg_port0 when "01",
                        data_in when "10",
                        operand when "11",
                        (others => '0') when others;
    alu0_mode <= control_signals.alu_mode;
    ALU0: ALU port map(
        operand_a   => reg_port1,
        operand_b   => alu0_inputb,
        mode        => alu0_mode,
        result      => alu0_result,
        flags_in    => alu0_flags,
        flags_out   => alu0_newflags,
        enable      => control_signals.en_alu
    );

    --data memory acces
    data_adres <= r4 & alu0_result; -- r4 is used as a banking register so we can acces more memory
    data_out <= reg_port0;
    bus_req <= control_signals.busrq;
    we <= control_signals.write;

    --control unit stuff
    program_adres <= std_logic_vector(instruction_pointer);
    opcode <= instruction_register(7 downto 0);
    operand <= instruction_register(15 downto 8);

    instruction_decoder: process (opcode)
    begin
        control_signals <= (
            set_reg => '0',
            en_alu => '0',
            sel_reg_inp => '0',
            sel_alu_inp => "00",
            alu_mode => "00",
            busrq => '0',
            write => '0',
            set_ip => '0'
        );
        case opcode(3 downto 0) is
            when "0000" => --add reg, reg
                control_signals.set_reg <= '1';
                control_signals.en_alu <= '1';
                control_signals.sel_alu_inp <= "01";
                control_signals.sel_reg_inp <= '1';
                control_signals.alu_mode <= "00";
            when "0001" => --addc reg, reg
                control_signals.set_reg <= '1';
                control_signals.en_alu <= '1';
                control_signals.sel_alu_inp <= "01";
                control_signals.sel_reg_inp <= '1';
                control_signals.alu_mode <= "01";
            when "0010" => --sub reg, reg
                control_signals.set_reg <= '1';
                control_signals.en_alu <= '1';
                control_signals.sel_alu_inp <= "01";
                control_signals.sel_reg_inp <= '1';
                control_signals.alu_mode <= "10";
            when "0011" => --subc reg, reg
                control_signals.set_reg <= '1';
                control_signals.en_alu <= '1';
                control_signals.sel_alu_inp <= "01";
                control_signals.sel_reg_inp <= '1';
                control_signals.alu_mode <= "11";
			when "0100" => --cmp reg, immediate
				control_signals.en_alu <= '1';
				control_signals.sel_alu_inp <= "11";
				control_signals.alu_mode <= "10";
			when "0101" => --cmp reg, reg
				control_signals.en_alu <= '1';
				control_signals.sel_alu_inp <= "01";
				control_signals.alu_mode <= "10";
			--when "0110" => --mul_lo
			--when "0111" => --mul_hi
			--when "1000" => --shr
            when "1001" => --ld reg, reg(operand)
                control_signals.set_reg <= '1';
                control_signals.en_alu <= '1';
                control_signals.sel_reg_inp <= '0';
                control_signals.sel_alu_inp <= "11";
                control_signals.busrq <= '1';
            when "1010" => --ld reg, operand
                control_signals.set_reg <= '1'; --set the register file in the writeback state
                control_signals.en_alu <= '0'; --this just means that the alu will act like it doesnt exist when the mode is set to "00"
                control_signals.sel_reg_inp <= '1'; --set the register file input to the alu
                control_signals.sel_alu_inp <= "11"; --this sets input b of the alu to the immediate value
            when "1011" => --mov reg, reg
                control_signals.set_reg <= '1';
                control_signals.en_alu <= '1';
                control_signals.sel_reg_inp <= '1';
                control_signals.sel_alu_inp <= "00";
            when "1100" => --str reg, reg(operand)
                control_signals.en_alu <= '1';
                control_signals.sel_alu_inp <= "11";
                control_signals.busrq <= '1';
                control_signals.write <= '1';
            when "1101" => --jmp operand
                control_signals.set_ip <= '1';
			--when "1110" => --umsk
			--when "1111" => --msk immediate
            when others =>
                null;
        end case;
    end process;

    control_unit: process (clk, reset)
    begin
		
        if rising_edge(clk) then
			if reset = '1' then
				cpu_state <= WB;
				
				instruction_pointer <= (others => '0');
				instruction_register <= (others => '0');
				r1 <= (others => '0');
				r2 <= (others => '0');
				r3 <= (others => '0');
				r4 <= (others => '0');
			else
				if cpu_state = FE then --fetch instruction
				
					instruction_register <= program_in;
					instruction_pointer <= instruction_pointer + 1;
					
					cpu_state <= EX;

				end if;

				if cpu_state = EX then --execute instruction
					alu0_flags <= alu0_newflags;
					cpu_state <= WB;
				end if;

				
				if cpu_state = WB then
					if control_signals.set_reg = '1' then
						case reg_select0 is
							when "00" => r1 <= reg_input;
							when "01" => r2 <= reg_input;
							when "10" => r3 <= reg_input;
							when "11" => r4 <= reg_input;
						end case;
					end if;

					if control_signals.set_ip = '1' then
						instruction_pointer <= unsigned(operand);
					end if;
					
					cpu_state <= FE;
				end if;
			end if;
        end if;
    end process;

end architecture SIMD_core_impl;








