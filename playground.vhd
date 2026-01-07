
LIBRARY ieee;                 -- this lib needed for STD_LOGIC
USE ieee.std_logic_1164.all;  -- the package with this info
USE ieee.numeric_std.all;     -- UNSIGNED

entity led7seg_decoder is

    port (
        input : in std_logic_vector(3 downto 0);
        segments : out std_logic_vector(7 downto 0) --7 segment display
    );

end entity led7seg_decoder;

architecture led7seg_decoder_arch of led7seg_decoder is
    
    --just stole this from stack overflow
    function reverse_vector(v : std_logic_vector) return std_logic_vector is
        variable r : std_logic_vector(v'range);
    begin
        for i in v'range loop
            r(i) := v(v'length - 1 - i);
        end loop;
        return r;
    end function;


    type segment_table is array(0 to 15) of std_logic_vector(6 downto 0);

    constant hex_table : segment_table := (
        "0000001", -- 0
        "1001111", -- 1
        "0010010", -- 2
        "0000110", -- 3
        "1001100", -- 4
        "0100100", -- 5
        "0100000", -- 6
        "0001111", -- 7
        "0000000", -- 8
        "0000100", -- 9
        "0001000", -- A
        "1100000", -- b
        "0110001", -- C
        "1000010", -- d
        "0110000", -- E
        "0111000"  -- F
    );

begin

    segments(6 downto 0) <= reverse_vector(hex_table(to_integer(unsigned(input))));
    segments(7) <= '1';

end architecture led7seg_decoder_arch;



LIBRARY ieee;                 -- this lib needed for STD_LOGIC
USE ieee.std_logic_1164.all;  -- the package with this info
USE ieee.numeric_std.all;     -- UNSIGNED

entity main is

    generic (
        CLOCK_FREQUENCY: integer := 50000000
    );

    port (
        MAX10_CLK1_50: IN STD_LOGIC;
        SW   : IN  STD_LOGIC_VECTOR(9 downto 0);  --! Switches
        KEY  : IN  STD_LOGIC_VECTOR(1 downto 0);  --! Keys
        LEDR : OUT STD_LOGIC_VECTOR(9 downto 0);  --! Leds
        HEX0,
        HEX1,
        HEX2,
        HEX3,
        HEX4,
        HEX5 : OUT STD_LOGIC_VECTOR(7 downto 0)   --! 7-signals to control leds in HEX-hexDisplay

    );

end entity main;


architecture main_arch of main is

    component led7seg_decoder is 
        port (
            input : in std_logic_vector(3 downto 0);
            segments : out std_logic_vector(7 downto 0) --7 segment display
        );
    end component led7seg_decoder;

    component test_cpu is
        port (
            data_in : in std_logic_vector(7 downto 0);
            data_out : out std_logic_vector(7 downto 0);
            adres_out : out std_logic_vector(7 downto 0);
            read_enable : out std_logic;
            write_enable : out std_logic;
            reset : in std_logic;
            clock : in std_logic;
            chip_select : in std_logic;
            reg_ir : inout unsigned(4 downto 0);
            reg_ic : inout unsigned(2 downto 0)
        );
    end component test_cpu;
    
    component SIMD_core is
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
            rst : in std_logic;
            clk : in std_logic
        );

    end component SIMD_core;

    component test_ram is

        port (
            data_in : in std_logic_vector(7 downto 0);
            data_out : out std_logic_vector(7 downto 0);
            adres_in : in std_logic_vector(7 downto 0);
            read_enable : in std_logic;
            write_enable : in std_logic;
            chip_select : in std_logic;
            clock : in std_logic
        );

    end component test_ram;
    component altclkctrl 
        generic (
            clock_type : string := "Global Clock";
            intended_device_family : string := "MAX 10"
        );
    
        port (
            inclk : in std_logic_vector(3 downto 0);
            ena : in std_logic; outclk : out std_logic
        );
    end component;

    component test_fsm is
        port (
            output : out std_logic_vector(7 downto 0);
            input : in std_logic_vector(7 downto 0);
            current_state : out std_logic_vector(7 downto 0);
            clk : in std_logic;
            reset : in std_logic
        );

    end component test_fsm;
    
    signal sys_clk : std_logic;

    signal cpu0_adres : std_logic_vector(15 downto 0); --master to slave
    signal cpu0_data_in : std_logic_vector(7 downto 0); --slave to master
    signal cpu0_data_out : std_logic_vector(7 downto 0); --master to slave
    signal cpu0_bus_req : std_logic;
    signal cpu0_we : std_logic;
    signal cpu0_rst : std_logic

    signal bram0_read : std_logic;
    signal bram0_write : std_logic;
    signal bram0_chip_select : std_logic;

    signal input : std_logic_vector(15 downto 0);
    signal address : std_logic_vector(7 downto 0);
    signal btn_sync : std_logic_vector(1 downto 0);
    signal btn_edge : std_logic;
begin

    bram0: test_ram port map(data_in => cpu0_data_out, data_out => cpu0_data_in, adres_in => cpu0_adres, read_enable => bram0_read, write_enable => bram0_write, chip_select => bram0_chip_select);

   

    cpu0: SIMD_core port map(data_in => cpu0_data_in, data_out => cpu0_data_out, data_adres => cpu0_adres, program_in => input, program_adres => address, bus_req => cpu0_bus_req, we => cpu0_we, rst => cpu0_rst, clk => sys_clk);
    

    bram0_chip_select <= '1';
    bram0_read <= cpu0_bus_req and not cpu0_we;
    bram0_write <= cpu0_bus_req and cpu0_we;
    
    LEDR <= address;
    
    input_test: process (KEY(0))

        if falling_edge(KEY(0)) then
            if SW(9) = '1' then --set upper byte
                input(15 downto 8) <= SW(7 downto 0);
            else --set lower byte
                input(7 downto 0) <= SW(15 downto 8);
            end if;
        end if;
    end process;

    btn_sync: process(MAX10_CLK1_50)
    begin
        if rising_edge(MAX10_CLK1_50) then
            btn_sync(0) <= button;
            btn_sync(1) <= btn_sync(0);
        end if;
    end process;

    btn_edge <= btn_sync(0) and not btn_sync(1); --check the edge

    clk_gate_inst : altclkctrl generic map (
        clock_type => "Global Clock",
        intended_device_family => "MAX 10"
    ) port map (
        inclk(0) => MAX10_CLK1_50,
        inclk(1) => '0',
        inclk(2) => '0',
        inclk(3) => '0',
        ena => btn_edge,
        outclk => sys_clk
    );

    /*
    devider : PROCESS (MAX10_CLK1_50)
    
        --! integer for counting delimited to 64 there for 6 lines on vector.
        VARIABLE counter : INTEGER RANGE 0 TO CLOCK_FREQUENCY/2 := 0;

    BEGIN


   
        IF rising_edge(MAX10_CLK1_50) THEN               -- on clock edge
      
            IF (counter < CLOCK_FREQUENCY/2) THEN  -- as long as the counter is below 64
                counter := counter + 1;              -- increment counter
            ELSE                                   -- as the counter reached 64
                counter := 0;                        -- reset counter to 0
                sys_clk <= not sys_clk; 
            END IF;
        END IF;                                   -- put result of counter on signal
    END PROCESS;

   */

end architecture main_arch;

