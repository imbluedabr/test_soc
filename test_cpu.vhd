
LIBRARY ieee;                 -- this lib needed for STD_LOGIC
USE ieee.std_logic_1164.all;  -- the package with this info
USE ieee.numeric_std.all;     -- UNSIGNED

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


LIBRARY ieee;                 -- this lib needed for STD_LOGIC
USE ieee.std_logic_1164.all;  -- the package with this info
USE ieee.numeric_std.all;     -- UNSIGNED

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

    signal internal_bus : unsigned(7 downto 0);

    type microcode_rom_type is array (0 to 31) of std_logic_vector(7 downto 0);
    
    constant microcode_rom : microcode_rom_type := (
        --0x00 NOP
        0  => "11000100", --set adres_out, select ip, inc ip
        1  => "00000000",
        2  => "00001000",
        others => (others => '0')
    );

begin

    cycle: process (clock, reset, chip_select)
    begin
        if (reset = '0') then
            reg_ip <= (others => '0');
            reg_ir <= (others => '0');
            reg_ic <= (others => '0');
        elsif rising_edge(clock) then --this doesnt run if enable is low 
            if (chip_select = '1') then
                
                control_signals <= microcode_rom(to_integer((reg_ir sll 3) or reg_ic));
                reg_ic <= reg_ic + 1; --increment the microinstruction counter


            end if;
        end if;
    end process;


end architecture test_cpu_arch;


