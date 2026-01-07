library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_fsm is

    generic (
        ldin    : std_logic_vector(7 downto 0) := "00000010";
        ldout   : std_logic_vector(7 downto 0) := "00001000";
        sel_in  : std_logic_vector(7 downto 0) := "00000001";
        rst_in  : std_logic_vector(7 downto 0) := "00000100"
    );

    port (
        output : out std_logic_vector(7 downto 0);
        input : in std_logic_vector(7 downto 0);
        current_state : out std_logic_vector(7 downto 0);
        clk : in std_logic;
        reset : in std_logic
    );

end entity test_fsm;


architecture arch of test_fsm is

    function transition(state, symbol: integer; ctrl: std_logic_vector) return std_logic_vector is
        variable r : std_logic_vector(23 downto 0);
    begin
        r(7 downto 0) := std_logic_vector(to_unsigned(symbol, 8));
        r(15 downto 8) := std_logic_vector(to_unsigned(state, 8));
        r(23 downto 16) := ctrl;
        return r;
    end function;

    function condition(state: integer; symbol: integer) return integer is
        variable r : std_logic_vector(15 downto 0);
    begin
        r := std_logic_vector(to_unsigned(state, 8)) & std_logic_vector(to_unsigned(symbol, 8));
        return to_integer(unsigned(r));
    end function;

    type state_t is array(0 to 65535) of std_logic_vector(23 downto 0);
    
    constant state_rom : state_t := (
        condition(0, 0) => transition(1, 0, sel_in),
        condition(1, 0) => transition(1, 1, ldout or ldin),
        condition(1, 1) => transition(1, 2, ldout or ldin),
        condition(1, 2) => transition(1, 3, ldout or ldin),
        condition(1, 3) => transition(1, 0, ldout or ldin),
        others => (others => '0')
    );

    signal input_symbol : unsigned(7 downto 0) := (others => '0');
    signal state_reg : unsigned(7 downto 0) := (others => '0');
begin
    
    current_state <= std_logic_vector(state_reg);

    fsm: process (clk)
        variable temp_state : std_logic_vector(23 downto 0);
        variable databus : unsigned(7 downto 0);
    begin

        if (rising_edge(clk)) then
            if (reset = '1') then
                state_reg <= (others => '0');
                input_symbol <= (others => '0');
                output <= (others => '0');
            else
                temp_state := state_rom(to_integer(state_reg & input_symbol));
                
                state_reg <= unsigned(temp_state(15 downto 8));

                --databus source select
                if (temp_state(16) = '1') then
                    databus := unsigned(input);
                else
                    databus := unsigned(temp_state(7 downto 0));
                end if;

                if (temp_state(17) = '1') then --set input symbol
                    input_symbol <= databus;
                elsif (temp_state(18) = '1') then --reset input symbol
                    input_symbol <= (others => '0');
                end if;

                if (temp_state(19) = '1') then --set output symbol
                    output <= std_logic_vector(databus);
                end if;

            end if;
        end if;
    end process fsm;

end architecture arch;


