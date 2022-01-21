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





module guest_mist
(       
        output        LED,                                              
        output        VGA_HS,
        output        VGA_VS,
        output        AUDIO_L,
        output        AUDIO_R, 
		  output [15:0]  DAC_L, 
		  output [15:0]  DAC_R, 
        input         UART_RX,
        output        UART_TX,
        input         SPI_SCK,
        output        SPI_DO,
        input         SPI_DI,
        input         SPI_SS2,
        input         SPI_SS3,
        input         CONF_DATA0,
        input         CLOCK_27,
        output  [5:0] VGA_R,
        output  [5:0] VGA_G,
        output  [5:0] VGA_B,

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
        output        SDRAM_CKE
);


//assign AUDIO_L = 0;
//assign AUDIO_R = 0;



//////////////////////////////////////////////////////////////////


wire scandoubler = (scale || forced_scandoubler);
wire [2:0] scale = status[5:3];



`include "build_id.v" 
localparam CONF_STR = {
	"TatungEinstein;;",
	"S0,DSK,Mount Disk 0:;",
 	//"S1,DSK,Mount Disk 1:;",
	//"O89,Aspect ratio,Original,Full Screen,[ARC1],[ARC2];",
	"O35,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",
	"O2,Joystick,Digital,Analog;",
	"O6,Diagnostic ROM mounted,Off,On;",
	"O7,Border,Off,On;",
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
	.c0(clk_sys), // 32
	.c1(clk_vdp), // 10
	.c2(clk_vid) // 40
);

wire reset =status[0] | buttons[1] | !locked;

reg [2:0] clk_div;
wire clk_cpu = clk_div[2]; // 4M
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
   .VBlank(vga_vblank)
);


endmodule
