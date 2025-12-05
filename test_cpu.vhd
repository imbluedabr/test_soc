library ieee;                 -- this lib needed for STD_LOGIC
use ieee.std_logic_1164.all;  -- the package with this info
use ieee.numeric_std.all;     -- UNSIGNED

entity test_ram is

    port (
        data_in : in std_logic_vector(7 downto 0);
        data_out : out std_logic_vector(7 downto 0);
        adres_in : in std_logic_vector(7 downto 0);
        read_enable : in std_logic;
        write_enable : in std_logic;
        chip_select : in std_logic;
        clock : in std_logic
    );

end entity test_ram;

architecture test_ram_arch of test_ram is

    type ram_type is array (0 to  255) of std_logic_vector(7 downto 0);

    signal ram : ram_type;

begin

    ram_p: process (clock)
    begin
        if rising_edge(clock) then
            if (chip_select = '1') then
                if (write_enable = '1') then
                    ram(to_integer(unsigned(adres_in))) <= data_in;
                end if;

                if (read_enable = '1') then
                    data_out <= ram(to_integer(unsigned(adres_in)));
                end if;
            end if;
        end if;
    end process;

end architecture test_ram_arch;


library ieee;                 -- this lib needed for STD_LOGIC
use ieee.std_logic_1164.all;  -- the package with this info
use ieee.numeric_std.all;     -- UNSIGNED

entity test_cpu is

    port (
        data_in : in std_logic_vector(7 downto 0);
        data_out : out std_logic_vector(7 downto 0);
        adres_out : out std_logic_vector(7 downto 0);
        read_enable : out std_logic;
        write_enable : out std_logic;
        reset : in std_logic;
        clock : in std_logic;
        chip_select : in std_logic;
        reg_ir : inout unsigned(4 downto 0); --debug outputs
        reg_ic : inout unsigned(2 downto 0)
    );

end entity test_cpu;


architecture test_cpu_arch of test_cpu is

    signal reg_ip : unsigned(7 downto 0);
    
    signal reg_mar : unsigned(7 downto 0);

    signal reg_a : unsigned(7 downto 0);
    signal reg_b : unsigned(7 downto 0);

    type microcode_rom_type is array (0 to 31) of std_logic_vector(10 downto 0);
    
    constant microcode_rom : microcode_rom_type := (
        --0x00 NOP
        0  => "00001110010", --ld adres, inc ip, select reg_ip, rd
        1  => "00000000000",
        2  => "00000001001", --ld ir, select data_in
        --0x01 JMP adres
        8  => "00001110010", --ld adres, inc ip, select reg_ip, rd
        9  => "00000000000",
        10 => "00010000001", --ld ip, select data_in
        11 => "00001110010", --ld adres, inc ip, select reg_ip, rd
        12 => "00000000000",
        13 => "00000001001", --ld ir, select data_in
        --0x02 LDA value
        16 => "00001110010", --select reg_ip, ld adres, inc ip, rd  = fetch operand
        17 => "00001110010", --select reg_ip, ld adres, inc ip, rd  = fetch next instr
        18 => "00100000001", --select data_in, ld a                 = store operand
        19 => "00000001001", --select data_in, ld ir                = execute next instruction
        --0x03 STA adres
        24 => "00001110010", --select reg_ip, ld adres, inc ip, rd  = fetch operand
        25 => "00000000000", 
        26 => "00000010001", --select data_in, ld adres             = store operand
        27 => "11000000100", --select reg_a, ld data_out, wr        = write data
        others => (others => '0')
    );

begin

    cycle: process (clock, reset, chip_select)
        variable control_signals : std_logic_vector(10 downto 0) := (others => '0');
        variable internal_bus : unsigned(7 downto 0) := (others => '0');
    begin
        if (reset = '0') then
            reg_ip <= (others => '0');
            reg_ir <= (others => '0');
            reg_ic <= (others => '0');
        elsif rising_edge(clock) then --this doesnt run if enable is low 
            if (chip_select = '1') then
                
                control_signals := microcode_rom(to_integer((reg_ir sll 3) or reg_ic));
                reg_ic <= reg_ic + 1; --increment the microinstruction counter

                --this is horrible
                case control_signals(2 downto 0) is
                    when "001" => internal_bus := unsigned(data_in);
                    when "010" => internal_bus := reg_ip;
                    when "011" => internal_bus := reg_mar;
                    when "100" => internal_bus := reg_a;
                    when "101" => internal_bus := reg_b;
                    when others => internal_bus := (others => '0');
                end case;

                if (control_signals(3) = '1') then --ld ir : loads a new instruction from the internal_bus
                    reg_ir <= internal_bus(4 downto 0);
                    reg_ic <= (others => '0');
                end if;

                if (control_signals(4) = '1') then --ld adres : sets the adres_out buffer to the internal_bus
                    adres_out <= std_logic_vector(internal_bus);
                end if;

                if (control_signals(5) = '1') then --inc ip : increments the instruction pointer
                    reg_ip <= reg_ip + 1;
                end if;

                if (control_signals(6) = '1') then --rd : enable read enable
                    read_enable <= '1';
                else
                    read_enable <= '0';
                end if;

                if (control_signals(7) = '1') then --ld ip
                    reg_ip <= internal_bus;
                end if;

                if (control_signals(8) = '1') then --ld a
                    reg_a <= internal_bus;
                end if;

                if (control_signals(9) = '1') then --ld data_out
                    data_out <= internal_bus;
                end if;

                if (control_signals(10) = '1') then --wr
                    write_enable <= '1';
                else
                    write_enable <= '0';
                end if;
            end if;
        end if;
    end process;


end architecture test_cpu_arch;


