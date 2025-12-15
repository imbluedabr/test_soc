library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity test_fsm is
    port (
        output : out std_logic_vector(7 downto 0);
        input : in std_logic_vector(7 downto 0);
        clk : in std_logic;
        reset : in std_logic
    );

end entity test_fsm;


architecture arch of test_fsm is

    function transition(state: integer; symbol: integer; ctrl: std_logic_vector(3 downto 0)) return std_logic_vector(19 downto 0) is
        variable r : std_logic_vector(19 downto 0);
    begin
        r(7 downto 0) := std_logic_vector(to_unsigned(symbol, 8));
        r(15 downto 8) := std_logic_vector(to_unsigned(state, 8));
        r(19 downto 16) := ctrl;
        return r;
    end function;

    function condition(state: integer; symbol: integer) return integer is
        variable r : std_logic_vector(15 downto 0);
    begin
        r := std_logic_vector(to_unsigned(state, 8)) & std_logic_vector(to_unsigned(symbol, 8));
        return to_integer(unsigned(r));
    end function;

    type state_t is array(0 to 65535) of std_logic_vector(19 downto 0);
    
    constant state_rom : state_t := (
        0 => transition(1, 0, "1101"),
        256 => transition(0, 1, "1110"),
        257 => transition(0, 0, "1110"),
        others => (others => '0')
    );

    signal current_state : std_logic_vector(7 downto 0) := (others => '0');
    signal input_symbol : std_logic_vector(7 downto 0) := (others => '0');
    
begin
    
    fsm: process (clk, input)
        variable temp_state : std_logic_vector(19 downto 0);
        variable databus : std_logic_vector(7 downto 0);
    begin

        temp_state := state_rom(current_state & input_symbol);
        
        --databus source select
        if (temp_state(16) = '1') then
            databus := input;
        else
            databus := temp_state(7 downto 0);
        end if;

        if (temp_state(17) = '1') then --set input symbol
            input_symbol <= databus;
        end if;

        if (temp_state(18) = '1') then --set output symbol
            output <= databus;
        end if;
        
        if (temp_state(19) = '1') then --set state
            current_state <= temp_state(15 downto 8);
        end if;

    end process fsm;

end architecture arch


