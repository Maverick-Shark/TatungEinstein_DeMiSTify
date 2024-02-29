//============================================================================
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
//============================================================================

module guest_top
(
	input         CLOCK_27,
`ifdef USE_CLOCK_50
	input         CLOCK_50,
`endif

	output        LED,
	output [VGA_BITS-1:0] VGA_R,
	output [VGA_BITS-1:0] VGA_G,
	output [VGA_BITS-1:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,

`ifdef USE_HDMI
	output        HDMI_RST,
	output  [7:0] HDMI_R,
	output  [7:0] HDMI_G,
	output  [7:0] HDMI_B,
	output        HDMI_HS,
	output        HDMI_VS,
	output        HDMI_PCLK,
	output        HDMI_DE,
	inout         HDMI_SDA,
	inout         HDMI_SCL,
	input         HDMI_INT,
`endif

	input         SPI_SCK,
	inout         SPI_DO,
	input         SPI_DI,
	input         SPI_SS2,    // data_io
	input         SPI_SS3,    // OSD
	input         CONF_DATA0, // SPI_SS for user_io

`ifdef USE_QSPI
	input         QSCK,
	input         QCSn,
	inout   [3:0] QDAT,
`endif
`ifndef NO_DIRECT_UPLOAD
	input         SPI_SS4,
`endif

	output [12:0] SDRAM_A,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nWE,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nCS,
	output  [1:0] SDRAM_BA,
	output        SDRAM_CLK,
	output        SDRAM_CKE,

`ifdef DUAL_SDRAM
	output [12:0] SDRAM2_A,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_DQML,
	output        SDRAM2_DQMH,
	output        SDRAM2_nWE,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nCS,
	output  [1:0] SDRAM2_BA,
	output        SDRAM2_CLK,
	output        SDRAM2_CKE,
`endif

	output        AUDIO_L,
	output        AUDIO_R,
`ifdef I2S_AUDIO
	output        I2S_BCK,
	output        I2S_LRCK,
	output        I2S_DATA,
`endif
`ifdef USE_AUDIO_IN
	input         AUDIO_IN,
`endif
	input         UART_RX,
	output        UART_TX

	output [15:0]  DAC_L,
	output [15:0]  DAC_R,
	
	output        vga_blank,
	output        vga_clk,
	output [5:0]  vga_x_r,
	output [5:0]  vga_x_g,
	output [5:0]  vga_x_b,
	output        vga_x_hs,
	output 	      vga_x_vs,

);

`ifdef NO_DIRECT_UPLOAD
localparam bit DIRECT_UPLOAD = 0;
wire SPI_SS4 = 1;
`else
localparam bit DIRECT_UPLOAD = 1;
`endif

`ifdef USE_QSPI
localparam bit QSPI = 1;
assign QDAT = 4'hZ;
`else
localparam bit QSPI = 0;
`endif

`ifdef VGA_8BIT
localparam VGA_BITS = 8;
`else
localparam VGA_BITS = 6;
`endif

`ifdef USE_HDMI
localparam bit HDMI = 1;
assign HDMI_RST = 1'b1;
`else
localparam bit HDMI = 0;
`endif

`ifdef BIG_OSD
localparam bit BIG_OSD = 1;
`define SEP "-;",
`else
localparam bit BIG_OSD = 0;
`define SEP
`endif

`ifdef USE_AUDIO_IN
localparam bit USE_AUDIO_IN = 1;
`else
localparam bit USE_AUDIO_IN = 0;
`endif
	
// remove this if the 2nd chip is actually used
`ifdef DUAL_SDRAM
assign SDRAM2_A = 13'hZZZZ;
assign SDRAM2_BA = 0;
assign SDRAM2_DQML = 0;
assign SDRAM2_DQMH = 0;
assign SDRAM2_CKE = 0;
assign SDRAM2_CLK = 0;
assign SDRAM2_nCS = 1;
assign SDRAM2_DQ = 16'hZZZZ;
assign SDRAM2_nCAS = 1;
assign SDRAM2_nRAS = 1;
assign SDRAM2_nWE = 1;
`endif

//////////////////////////////////////////////////////////////////

wire scandoubler = (scale || forced_scandoubler);
wire [2:0] scale = status[5:3];

`include "build_id.v"

localparam CONF_STR = {
	"TatungEinstein;;",
	"S0,DSK,Mount Disk 0:;",
 	//"S1,DSK,Mount Disk 1:;",
	//"O89,Aspect ratio,Original,Full Screen,[ARC1],[ARC2];",
	`SEP
	"O35,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",
	"O2,Joystick,Digital,Analog;",
	"O6,Diagnostic ROM mounted,Off,On;",
	"O7,Border,Off,On;",
	`SEP
	"T0,Reset;",
	"V,v",`BUILD_DATE 
};

wire forced_scandoubler;
wire  [1:0] buttons;
wire [31:0] status;
wire [10:0] ps2_key;
wire [31:0] sd_lba;
wire  [1:0] sd_rd;
wire  [1:0] sd_wr;
wire        sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire  [1:0] img_mounted;
wire  [1:0] img_readonly;
wire [31:0] img_size;

wire [7:0] joystick_0;
wire [7:0] joystick_1;
wire [15:0] joystick_analog_0;
wire [15:0] joystick_analog_1;

mist_io #(.STRLEN($size(CONF_STR)>>3),.PS2DIV(100)) mist_io
(
	.clk_sys(clk_sys),

	.SPI_SCK   (SPI_SCK),
   .CONF_DATA0(CONF_DATA0),
   .SPI_SS2   (SPI_SS2),
   .SPI_DO    (SPI_DO),
   .SPI_DI    (SPI_DI),
	
	.scandoubler_disable(forced_scandoubler),

	.buttons(buttons),
	.status(status),
	
	.ps2_key(ps2_key),

   .conf_str(CONF_STR),
	
	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr),

	.img_mounted(img_mounted),
	//.img_readonly(img_readonly),
	.img_size(img_size),

	.joystick_0(joystick_0),
	.joystick_1(joystick_1)
	//.joystick_l_analog_0(joystick_analog_0),
	//.joystick_l_analog_1(joystick_analog_1)
);

///////////////////////   CLOCKS   ///////////////////////////////

wire clk_sys;
wire clk_vdp;
wire clk_vid;
wire locked;

pll pll
(
	.inclk0(CLOCK_27),
	.areset(1'b0),
	.locked(locked),
	.c0(clk_sys), // 32 MHz
	.c1(clk_vdp), // 10 MHz
	.c2(clk_vid)  // 40 MHz
);

wire reset =status[0] | buttons[1] | !locked;

reg [2:0] clk_div;
wire clk_cpu = clk_div[2];        // 4 M
wire clk_fdc = clk_div == 3'b111;
always @(posedge clk_sys) clk_div <= clk_div + 3'd1;

//////////////////////////////////////////////////////////////////

wire [7:0] kb_row;
wire [7:0] kb_col;
wire shift, ctrl, graph;
wire press_btn;

keyboard keyboard(
  .clk_sys(clk_sys),
  .reset(reset),
  .ps2_key(ps2_key),
  .addr(kb_row),
  .kb_cols(kb_col),
  .modif({ ctrl, graph, shift }),
  .press_btn(press_btn)
);

wire [9:0] sound;

assign DAC_L = { sound, 5'd0 };
assign DAC_R = { sound, 5'd0 };

dac #(10) dac_l (
   .clk_i        (clk_sys),
   .res_n_i      (1      ),
   .dac_i        (sound),
   .dac_o        (AUDIO_L)
);

assign AUDIO_R=AUDIO_L;

tatung tatung
(
	.clk_sys(clk_sys),
	.clk_vdp(clk_vdp),
	.clk_cpu(clk_cpu),
	.clk_fdc(clk_fdc),
	.reset(reset),

	.vga_red(vga_red),
	.vga_green(vga_green),
	.vga_blue(vga_blue),
	.vga_hblank(vga_hblank),
	.vga_vblank(vga_vblank),
	.vga_hsync(vga_hsync),
	.vga_vsync(vga_vsync),
	
	.sound(sound),
	
	.kb_row(kb_row),
	.kb_col(kb_col),
	.kb_shift(shift),
	.kb_ctrl(ctrl),
	.kb_graph(graph),
	.kb_down(press_btn),

	.img_mounted(img_mounted),
	.img_readonly(img_readonly),
	.img_size(img_size),

	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(|sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_dout(sd_buff_dout),
	.sd_din(sd_buff_din),
	.sd_dout_strobe(sd_buff_wr),

	.joystick_0(joystick_0),
	.joystick_1(joystick_1),
	.joystick_analog_0(joystick_analog_0),
   	.joystick_analog_1(joystick_analog_1),

	.diagnostic(status[6]),
	.border(status[7]),
	.analog(status[2])
);

wire [7:0] vga_red, vga_green, vga_blue;
wire vga_hsync, vga_vsync;
wire vga_hblank, vga_vblank;

//assign CLK_VIDEO = clk_vid;
wire ce_pix = pxcnt[2];
reg [2:0] pxcnt;

always @(posedge clk_vid)
	pxcnt <= pxcnt + 3'd1;

/////// Video

mist_video #(.COLOR_DEPTH(4), .OSD_COLOR(3'd5), .SD_HCNT_WIDTH(10), .OUT_COLOR_DEPTH(VGA_BITS), .BIG_OSD(BIG_OSD)) mist_video (
    .clk_sys     ( clk_sys    ),

    // OSD SPI interface
    .SPI_SCK     ( SPI_SCK    ),
    .SPI_SS3     ( SPI_SS3    ),
    .SPI_DI      ( SPI_DI     ),

    // scanlines (00-none 01-25% 10-50% 11-75%)
    .scanlines   ( st_scanlines  ),

    // non-scandoubled pixel clock divider 0 - clk_sys/4, 1 - clk_sys/2
    .ce_divider  ( 1'b0       ),

    // 0 = HVSync 31KHz, 1 = CSync 15KHz
    .scandoubler_disable ( scandoubler_disable ),
    // disable csync without scandoubler
    .no_csync    ( no_csync   ),
    // YPbPr always uses composite sync
    .ypbpr       ( ypbpr      ),
    // Rotate OSD [0] - rotate [1] - left or right
    .rotate      ( 2'b00      ),
    // composite-like blending
    .blend       ( st_blend   ),

    // video in
    .R           ( R_O        ),
    .G           ( G_O        ),
    .B           ( B_O        ),

    .HSync       ( HS_O       ),
    .VSync       ( VS_O       ),

    // MiST video output signals
    .VGA_R       ( VGA_R      ),
    .VGA_G       ( VGA_G      ),
    .VGA_B       ( VGA_B      ),
    .VGA_VS      ( vs         ),
    .VGA_HS      ( hs         )
);

// Use different alignment of csync @15kHz
wire   cs = ~(~HS_O | ~VS_O);
assign VGA_HS = (~no_csync & scandoubler_disable & ~ypbpr) ? cs : hs;
assign VGA_VS = (~no_csync & scandoubler_disable & ~ypbpr) ? 1'b1 : vs;

`ifdef USE_HDMI
i2c_master #(35_480_000) i2c_master (
	.CLK         (clk_sys),
	.I2C_START   (i2c_start),
	.I2C_READ    (i2c_read),
	.I2C_ADDR    (i2c_addr),
	.I2C_SUBADDR (i2c_subaddr),
	.I2C_WDATA   (i2c_dout),
	.I2C_RDATA   (i2c_din),
	.I2C_END     (i2c_end),
	.I2C_ACK     (i2c_ack),

	//I2C bus
	.I2C_SCL     (HDMI_SCL),
	.I2C_SDA     (HDMI_SDA)
);

mist_video #(.COLOR_DEPTH(4), .OSD_COLOR(3'd5), .SD_HCNT_WIDTH(10), .OUT_COLOR_DEPTH(8), .USE_BLANKS(1'b1), .BIG_OSD(BIG_OSD), .VIDEO_CLEANER(1'b1)) hdmi_video (
    .clk_sys     ( clk_sys    ),

    // OSD SPI interface
    .SPI_SCK     ( SPI_SCK    ),
    .SPI_SS3     ( SPI_SS3    ),
    .SPI_DI      ( SPI_DI     ),

    // scanlines (00-none 01-25% 10-50% 11-75%)
    .scanlines   ( st_scanlines  ),

    // non-scandoubled pixel clock divider 0 - clk_sys/4, 1 - clk_sys/2
    .ce_divider  ( 1'b0       ),

    // 0 = HVSync 31KHz, 1 = CSync 15KHz
    .scandoubler_disable ( 1'b0 ),
    // disable csync without scandoubler
    .no_csync    ( 1'b1       ),
    // YPbPr always uses composite sync
    .ypbpr       ( 1'b0       ),
    // Rotate OSD [0] - rotate [1] - left or right
    .rotate      ( 2'b00      ),
    // composite-like blending
    .blend       ( st_blend   ),

    // video in
    .R           ( R_O        ),
    .G           ( G_O        ),
    .B           ( B_O        ),

    .HSync       ( HS_O       ),
    .VSync       ( VS_O       ),
    .HBlank      ( ~DE_O      ),
    .VBlank      ( ~VS_O      ),

    // MiST video output signals
    .VGA_R       ( HDMI_R     ),
    .VGA_G       ( HDMI_G     ),
    .VGA_B       ( HDMI_B     ),
    .VGA_VS      ( HDMI_VS    ),
    .VGA_HS      ( HDMI_HS    ),
    .VGA_DE      ( HDMI_DE    )
);
assign HDMI_PCLK = clk_sys;

`endif

video_mixer  video_mixer
(
   .*,

   .clk_sys(clk_sys),
   .ce_pix(ce_pix),
   .ce_pix_actual(ce_pix),
   .hq2x(scale==1),
	
   .scanlines(forced_scandoubler ? 2'b00 : {scale==3, scale==2}),
   .scandoubler_disable(forced_scandoubler),
   .ypbpr(),
   .ypbpr_full(),
   .mono(0),
   .line_start(),

   .R(vga_red[7:2]),
   .G(vga_green[7:2]),
   .B(vga_blue[7:2]),

   .HSync(vga_hsync),
   .VSync(vga_vsync),
   .HBlank(vga_hblank),
   .VBlank(vga_vblank),

   //added for HDMI output
   .vga_x_r(vga_x_r),
   .vga_x_g(vga_x_g),
   .vga_x_b(vga_x_b)

);

//added for HDMI output
assign vga_blank = vga_hblank | vga_vblank;     
assign vga_clk = clk_sys;     
assign vga_x_hs = vga_hsync;
assign vga_x_vs = vga_vsync;

endmodule
