set sysclk ${topmodule}pll|altpll_component|auto_generated|pll1|clk[0]
set vdpclk ${topmodule}pll|altpll_component|auto_generated|pll1|clk[1]
set vidclk ${topmodule}pll|altpll_component|auto_generated|pll1|clk[2]

create_clock -name clk_div_clk -period 250.000 {TatungEinstein_mist:guest|clk_div[2]}
#create_clock -name I054c_clk -period 250.000  {TatungEinstein_mist:guest|tatung:tatung|I054c}
#create_clock -name IORQ_n_clk -period 250.000  {TatungEinstein_mist:guest|tatung:tatung|T80s:t80s|IORQ_n}
#create_clock -name s_index_clk -period 250.000  {TatungEinstein_mist:guest|tatung:tatung|wd1793:fdc|s_index}

# Clock groups
set_clock_groups -asynchronous -group [get_clocks spiclk] -group [get_clocks $sysclk]
set_clock_groups -asynchronous -group [get_clocks $hostclk] -group [get_clocks $sysclk]
set_clock_groups -asynchronous -group [get_clocks $supportclk] -group [get_clocks $sysclk]

set_clock_groups -asynchronous -group [get_clocks spiclk] -group [get_clocks $vdpclk]
set_clock_groups -asynchronous -group [get_clocks $hostclk] -group [get_clocks $vdpclk]
set_clock_groups -asynchronous -group [get_clocks $supportclk] -group [get_clocks $vdpclk]

set_clock_groups -asynchronous -group [get_clocks spiclk] -group [get_clocks $vidclk]
set_clock_groups -asynchronous -group [get_clocks $hostclk] -group [get_clocks $vidclk]
set_clock_groups -asynchronous -group [get_clocks $supportclk] -group [get_clocks $vidclk]


# Some relaxed constrain to the VGA pins. The signals should arrive together, the delay is not really important.
set_output_delay -clock [get_clocks $sysclk] -max 0 [get_ports $VGA_OUT]
set_output_delay -clock [get_clocks $sysclk] -min -5 [get_ports $VGA_OUT]

set_multicycle_path -to $VGA_OUT -setup 2
set_multicycle_path -to $VGA_OUT -hold 1

set_false_path -to ${FALSE_OUT}
set_false_path -from ${FALSE_IN}








