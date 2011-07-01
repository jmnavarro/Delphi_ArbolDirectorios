//~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
//
// Unidad: ArbolDirectorios.pas
//
// Propósito:
//    Un componente visual que permite mostrar un árbol de directorios a partir de una carpeta
//    dada, o bien mostrar el típico árbol de "Mi PC", mostrando unidades y carpetas.
//
//
// Autor:          José Manuel Navarro (www.lawebdejm.com)
//
// Fecha:          01/04/2004
//
// Observaciones:  Unidad creada en Delphi 5 para la revista Todo Programación, editada por
//                 Studio Press, S.L. (www.iberprensa.com)
//
// Copyright:      Este código es de dominio público y se puede utilizar y/o mejorar siempre que
//                 SE HAGA REFERENCIA AL AUTOR ORIGINAL, ya sea a través de estos comentarios
//                 o de cualquier otro modo.
//
//~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
unit ArbolDirectorios;

interface

uses Classes, ComCtrls, SysUtils;

type
	TCustomArbolDirectorios = class(TCustomTreeView)
	private
		FMostrarPadre: boolean;
		FCarpetaRaiz: string;

		procedure SetMostrarPadre(value: boolean);
		procedure SetCarpetaRaiz(value: string);

		function  GetCarpetaActual: string;
	protected
		property MostrarPadre: boolean read FMostrarPadre write SetMostrarPadre;
		property CarpetaRaiz: string read FCarpetaRaiz write SetCarpetaRaiz;
		property CarpetaActual: string read GetCarpetaActual;

		procedure CargarSubcarpetas(carpeta: string; padre: TTreeNode);
		procedure CargarUnidades(nodoMiPC: TTreeNode);

		function CanExpand(Node: TTreeNode): Boolean; override;

		function GetRutaNodo(Nodo: TTreeNode; separador: char = '\'): string;

	public
		constructor Create(AOwner: TComponent); override;

		procedure Cargar();
	end;



	TArbolDirectorios = class(TCustomArbolDirectorios)
	published
		property Align;
		property Anchors;
		property AutoExpand;
		property BorderStyle;
		property ChangeDelay;
		property HideSelection;
		property HotTrack;
		property Images;
		property Indent;
		property RightClickSelect;

//		property OnCancelEdit;
		property OnChange;
		property OnChanging;
		property OnCollapsed;
		property OnCollapsing;
		property OnDeletion;
		property OnEditing;
		property OnEdited;
		property OnExpanding;
		property OnExpanded;
		property OnGetImageIndex;
		property OnGetSelectedIndex;

		property MostrarPadre;
		property CarpetaRaiz;
		property CarpetaActual;
	end;



procedure Register;


implementation

uses Windows;

const
	ICO_BASE = 0;

	ICO_MIPC            = ICO_BASE;
	ICO_DISQUETTE       = ICO_BASE + 1;
	ICO_DISCO_EXTRAIBLE = ICO_BASE + 2;
	ICO_DISCO_FIJO      = ICO_BASE + 3;
	ICO_DISCO_RED       = ICO_BASE + 4;
	ICO_DISCO_CDROM     = ICO_BASE + 5;
	ICO_DISCO_RAM       = ICO_BASE + 6;

	ICO_CARPETA_CERRADA = ICO_BASE + 7;
	ICO_CARPETA_ABIERTA = ICO_BASE + 8;


resourcestring
	SMiPC = 'Mi PC';


procedure Register;
begin
	RegisterComponents('Todo Programación', [TArbolDirectorios]);
end;



function ExisteCarpeta(carpeta: string): boolean;
var
  ret: integer;
begin
  ret := GetFileAttributes(PChar(carpeta));
  result := (ret <> -1) and (FILE_ATTRIBUTE_DIRECTORY and ret <> 0);
end;



constructor TCustomArbolDirectorios.Create(AOwner: TComponent);
begin
	inherited;

	FMostrarPadre := true;
	FCarpetaRaiz := '';
end;


procedure TCustomArbolDirectorios.CargarSubcarpetas(carpeta: string; padre: TTreeNode);
var
	datos: WIN32_FIND_DATA;
	busqueda: THandle;
	lista: TStringList;
	i: integer;
	nodo: TTreeNode;
begin
	carpeta := carpeta + '*.*';

	// primero cargamos en una lista ordenada los elementos.
	lista := TStringList.Create;
	try
		lista.Sorted := true;

		busqueda := FindFirstFile(PChar(carpeta), datos);
		if busqueda <> INVALID_HANDLE_VALUE then
		begin
			repeat
				if (datos.cFileName[0] <> '.') and
				   (datos.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0) then
				begin
					lista.Add(datos.cFileName);
				end;
			until not FindNextFile(busqueda, datos);

			Windows.FindClose(busqueda);
		end;

		// ahora recorremos la lista, e insertamos los nodos correspondientes
		for i:=0 to Pred(lista.Count) do
		begin
			nodo := Self.Items.AddChild(padre, lista[i]);
			nodo.HasChildren   := true;
			nodo.ImageIndex    := ICO_CARPETA_CERRADA;
			nodo.SelectedIndex := ICO_CARPETA_ABIERTA;
		end;

	finally
		lista.Free;
	end;
end;


procedure TCustomArbolDirectorios.CargarUnidades(nodoMiPC: TTreeNode);
var
	tipo: LongWord;
	mascara, unidades: DWORD;
	u: char;
	buff: array[0..2] of char;
	nodoUnidad: TTreeNode;

	procedure SetIconosNodo(nodo: TTreeNode; ico: integer);
	begin
		nodo.ImageIndex    := ico;
		nodo.SelectedIndex := ico;
	end;

begin
	unidades := GetLogicalDrives;
	mascara := 1;

	buff[0] := '?';
	buff[1] := ':';
	buff[2] := #0;

	for u:='A' to 'Z' do
	begin
		if (unidades and mascara) <> 0 then
		begin
			buff[0] := u;

			if ((tipo <> 0) and (tipo <> 1)) then
			begin
				nodoUnidad := Self.Items.AddChild(nodoMiPC, buff);
				nodoUnidad.HasChildren := true;

				if (u = 'A') or (u = 'B') then
					SetIconosNodo(nodoUnidad, ICO_DISQUETTE)
				else if (u = 'C') then
					SetIconosNodo(nodoUnidad, ICO_DISCO_FIJO)
				else
				begin
					tipo := GetDriveType(buff);

					case tipo of
						DRIVE_REMOVABLE:
							SetIconosNodo(nodoUnidad, ICO_DISCO_EXTRAIBLE);
						DRIVE_FIXED:
							SetIconosNodo(nodoUnidad, ICO_DISCO_FIJO);
						DRIVE_REMOTE:
							SetIconosNodo(nodoUnidad, ICO_DISCO_RED);
						DRIVE_CDROM:
							SetIconosNodo(nodoUnidad, ICO_DISCO_CDROM);
						else
							SetIconosNodo(nodoUnidad, ICO_DISCO_RAM);
					end;
				end;
			end;
		end;

		mascara := mascara shl 1; // desplazar hacia la izquierda
	end;
end;


procedure TCustomArbolDirectorios.Cargar();
var
	nodoPadre: TTreeNode;
	icono1, icono2: integer;
	nombre: string;
	len: integer;
begin
	if (csDesigning in ComponentState) or (FCarpetaRaiz = '') then
		exit;

	Self.Items.BeginUpdate;
	try
		Self.Items.Clear;

		if FMostrarPadre then
		begin
			nombre := FCarpetaRaiz;
			len := Length(nombre);
			if (nombre[len] = '\') and (len > 3) then
				nombre := Copy(nombre, 1, len-1);

			nodoPadre := Self.Items.Add(nil, nombre);
		end
		else
			nodoPadre := nil;

		if FCarpetaRaiz = SMiPC then
		begin
			icono1 := ICO_MIPC;
			icono2 := ICO_MIPC;

			CargarUnidades(nodoPadre);
		end
		else
		begin
			icono1 := ICO_CARPETA_CERRADA;
			icono2 := ICO_CARPETA_ABIERTA;

			CargarSubcarpetas(FCarpetaRaiz, nodoPadre);
		end;

		if nodoPadre <> nil then
		begin
			nodoPadre.ImageIndex    := icono1;
			nodoPadre.SelectedIndex := icono2;

			nodoPadre.Expanded := true;
		end;
	finally
		Self.Items.EndUpdate;
	end;
end;


function TCustomArbolDirectorios.CanExpand(Node: TTreeNode): Boolean;
var
	carpeta: string;
begin
	result := inherited CanExpand(Node);

	if result and Node.HasChildren and (Node.Count = 0) then
	begin
		carpeta := GetRutaNodo(Node);

		Self.Items.BeginUpdate;
		try
			CargarSubcarpetas(carpeta, Node);
			Node.HasChildren := (Node.Count > 0);
		finally
			Self.Items.EndUpdate;
		end;
	end;
end;


function TCustomArbolDirectorios.GetRutaNodo(Nodo: TTreeNode; separador: char = '\'): string;
var
	aux: string;
begin
	result := '';
	while Nodo <> nil do
	begin
		if Nodo.Text <> SMiPC then
      begin
      	aux := Nodo.Text;
         if aux[Length(aux)] = separador then
         	aux := Copy(aux, 1, Length(aux)-1);
            
			result := aux + separador + result;
      end;
		Nodo := Nodo.Parent;
	end;

	if (not FMostrarPadre) and (FCarpetaRaiz <> SMiPC) then
		result := FCarpetaRaiz + result;
end;


procedure TCustomArbolDirectorios.SetMostrarPadre(value: boolean);
begin
	if value <> FMostrarPadre then
	begin
		FMostrarPadre := value;
		Self.Cargar;
	end;
end;


procedure TCustomArbolDirectorios.SetCarpetaRaiz(value: string);
var
	len: integer;
begin
	value := Trim(value);
	if value <> FCarpetaRaiz then
	begin
		len := Length(value);
		if (value <> SMiPC) and (len > 0) then
		begin
			if value[len] <> '\' then
				value := value + '\';

			if not ExisteCarpeta(value) then
				raise Exception.Create('La carpeta indicada no existe. Puedes utilizar cualquier ruta de carpeta válida, o bien el valor "Mi PC".');
		end;

		FCarpetaRaiz := value;
		Self.Cargar;
	end;
end;


function  TCustomArbolDirectorios.GetCarpetaActual: string;
begin
	if (Self.Selected = Self.Items[0]) and (Self.Selected.Text = SMiPC) then
		result := SMiPC
	else
		result := GetRutaNodo(Self.Selected);
end;


end.
