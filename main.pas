unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ComCtrls, ArbolDirectorios, ExtCtrls, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    ArbolDirectorios1: TArbolDirectorios;
    icosW2K: TImageList;
    icosWXP: TImageList;
    Label1: TLabel;
    Edit1: TEdit;
    rb_w2k: TRadioButton;
    rb_wxp: TRadioButton;
    Label2: TLabel;
    Bevel1: TBevel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    Edit2: TEdit;
    SpeedButton1: TSpeedButton;
    procedure CambiaIconos(Sender: TObject);
    procedure ArbolDirectorios1Change(Sender: TObject; Node: TTreeNode);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CambiaIconos(Sender: TObject);
begin
	if rb_w2k.Checked then
		ArbolDirectorios1.Images := icosW2K
   else
		ArbolDirectorios1.Images := icosWXP;
end;

procedure TForm1.ArbolDirectorios1Change(Sender: TObject; Node: TTreeNode);
begin
	Edit1.Text := ArbolDirectorios1.CarpetaActual;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
	ArbolDirectorios1.MostrarPadre := CheckBox1.Checked;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
	ArbolDirectorios1.CarpetaRaiz := Edit2.Text;
end;

end.
