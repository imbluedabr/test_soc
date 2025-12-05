
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

    signal sys_clk : std_logic;
    signal sys_reset : std_logic;
    signal sys_data_in : std_logic_vector(7 downto 0);
    signal sys_adres_in : std_logic_vector(7 downto 0);
    signal sys_data_out : std_logic_vector(7 downto 0);
    signal sys_read_enable : std_logic;
    signal sys_write_enable : std_logic;

    signal cpu0_data_in : std_logic_vector(7 downto 0);
    signal cpu0_data_out : std_logic_vector(7 downto 0);
    signal cpu0_adres_out : std_logic_vector(7 downto 0);
    signal cpu0_read_enable : std_logic;
    signal cpu0_write_enable : std_logic;
    signal cpu0_chip_select : std_logic;
    signal cpu0_ir : unsigned(4 downto 0);
    signal cpu0_ic : unsigned(2 downto 0);

    signal programmer_data_in : std_logic_vector(7 downto 0);
    signal programmer_data_out : std_logic_vector(7 downto 0);
    signal programmer_adres_out : std_logic_vector(7 downto 0);
    signal programmer_read_enable : std_logic;
    signal programmer_write_enable : std_logic;
    signal programmer_enable : std_logic;

    signal mem0_data_out : std_logic_vector(7 downto 0);
    signal mem0_chip_select : std_logic;

    signal io0_data_register : std_logic_vector(7 downto 0);
    signal io0_chip_select : std_logic;

    signal display2_mux : std_logic_vector(3 downto 0);
    signal display3_mux : std_logic_vector(3 downto 0);

begin


    --master output mux
    sys_data_in <= cpu0_data_out when cpu0_chip_select = '1' else
                    programmer_data_out;
    --slave output mux
    sys_data_out <= mem0_data_out when mem0_chip_select = '1' else (others => '0');
    --slave input mux
    sys_adres_in <= cpu0_adres_out when cpu0_chip_select = '1' else
                    programmer_adres_out;
    sys_read_enable <= cpu0_read_enable when cpu0_chip_select = '1' else
                    programmer_read_enable;
    sys_write_enable <= cpu0_write_enable when cpu0_chip_select = '1' else
                    programmer_write_enable;

    --master input mux
    programmer_data_in <= sys_data_out when programmer_enable = '1' else (others => '0');
    cpu0_data_in <= sys_data_out when cpu0_chip_select = '1' else (others => '0');

    --bus arbitration logic
    programmer_enable <= SW(9);
    cpu0_chip_select <= not programmer_enable;
    
    sys_reset <= KEY(0);
    
    programmer: process (sys_clk, SW(8), KEY(1))
    begin
        if rising_edge(sys_clk) then
            if (SW(8) = '1') then
                programmer_data_out <= SW(7 downto 0);
            else
                programmer_adres_out <= SW(7 downto 0);
            end if;
            
            if (KEY(1) = '0') then --keys use active low logic(pull up resistors)
                programmer_write_enable <= '1';
                programmer_read_enable <= '0';
            else
                programmer_read_enable <= '1';
                programmer_write_enable <= '0';
            end if;
        end if;
    end process;

    io0_chip_select <= '1' when sys_adres_in = "10000000" else '0';
    io0: process (sys_clk)
    begin
        if rising_edge(sys_clk) then
            if (io0_chip_select = '1') then
                if (sys_write_enable = '1') then
                    io0_data_register <= sys_data_in;
                end if;
            end if;
        end if;
    end process;

    cpu0: test_cpu port map ( data_in => cpu0_data_in, data_out => cpu0_data_out, adres_out => cpu0_adres_out, read_enable => cpu0_read_enable, write_enable => cpu0_write_enable, reset => sys_reset, clock => sys_clk, chip_select => cpu0_chip_select, reg_ir => cpu0_ir, reg_ic => cpu0_ic );

    --chip select logic for memory, maps ram from 0 to 127
    mem0_chip_select <= '1' when sys_adres_in(7) = '0' else '0';
    mem0: test_ram port map ( data_in => sys_data_in, data_out => mem0_data_out, adres_in => sys_adres_in, read_enable => sys_read_enable, write_enable => sys_write_enable, clock => sys_clk, chip_select => mem0_chip_select);

    

    LEDR(9) <= sys_clk;
    LEDR(8) <= sys_read_enable;
    LEDR(7 downto 0) <= sys_adres_in;

    display0: led7seg_decoder port map ( input => std_logic_vector(cpu0_ir(3 downto 0)), segments => HEX0);

    display1: led7seg_decoder port map ( input => std_logic_vector(resize(cpu0_ic, 4)), segments => HEX1);

    display2_mux <= programmer_data_out(3 downto 0) when programmer_enable = '1' else
                    io0_data_register(3 downto 0);
    display3_mux <= programmer_data_out(7 downto 4) when programmer_enable = '1' else
                    io0_data_register(7 downto 4);

    display2: led7seg_decoder port map ( input => display2_mux, segments => HEX2);

    display3: led7seg_decoder port map ( input => display3_mux, segments => HEX3);

    display4: led7seg_decoder port map ( input => programmer_data_in(3 downto 0), segments => HEX4);

    display5: led7seg_decoder port map ( input => programmer_data_in(7 downto 4), segments => HEX5);


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

   

end architecture main_arch;

