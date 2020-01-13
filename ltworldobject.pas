unit ltworldobject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ltworldtypes, contnrs;

type

  { TLTWorldObjectProperty }

  TLTWorldObjectProperty = class(TObject)
  private
    m_nNameLength: Word;
    m_szName: string;
    m_nPropCode: Byte;
    m_nPropFlags: Cardinal;
    m_nPropDataLength: Word;
    {  PT_STRING = 0;
     PT_VECTOR = 1;
     PT_COLOR = 2;
     PT_REAL = 3;
     PT_FLAGS = 4;
     PT_BOOL = 5;
     PT_LONGINT = 6;
     PT_ROTATION = 7;  }
     m_szData: string;
     m_vData: LTVector;
     m_fData: LTFloat;
     m_nData: Byte;
     m_dwData: Cardinal;
     m_rData: LTRotation;
  protected
  public
    property DataStr: string read m_szData write m_szData;
    property DataVec: LTVector read m_vData write m_vData;
    property DataReal: LTFloat read m_fData write m_fData;
    property DataBool: Byte read m_nData write m_nData;
    property DataInt: Cardinal read m_dwData write m_dwData;
    property DataRot: LTRotation read m_rData write m_rData;
    function DataGeneric: string;

    property PropCode: Byte read m_nPropCode write m_nPropCode;
    property PropName: string read m_szName write m_szName;
    procedure ReadProperty(FS: TMemoryStream);
    procedure WriteProperty(nCode: Byte; szName: string; pData: Pointer);
    constructor Create; virtual;
    destructor Destroy; override;
  end;


  TLTWorldObject = class(TObject)
  private
    m_nDataLength: Word;
    m_nTypeStringLen: Word;
    m_szTypeString: string;
    m_nNumProperties: Cardinal;
    m_pPropertyList: TFPObjectList;
  protected
  public
    property DataLength: Word read m_nDataLength write m_nDataLength;
    property TypeStringLen: Word read m_nTypeStringLen write m_nTypeStringLen;
    property TypeString: string read m_szTypeString write m_szTypeString;
    property NumProperties: Cardinal read m_nNumProperties write m_nNumProperties;
    property PropertyList: TFPObjectList read m_pPropertyList;
    procedure WritePropString(szName: string; szData: string);
    procedure WritePropVector(szName: string; vData: LTVector);
    procedure WritePropColor(szName: string; vData: LTVector);
    procedure WritePropReal(szName: string; fData: LTFloat);
    procedure WritePropLongInt(szName: string; dwData: Cardinal);
    procedure WritePropFlags(szName: string; dwData: Cardinal);
    procedure WritePropBool(szName: string; nData: Byte);
    procedure WritePropRotation(szName: string; rData: LTRotation);
    function GetObjectName: string;
    procedure CreateProperty(nCode: Byte; szName: string; pData: Pointer);
    procedure ReadObject(FS: TMemoryStream);
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

procedure TLTWorldObject.WritePropString(szName: string; szData: string);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(PT_STRING, szName, @szData);
  m_pPropertyList.Add(pProperty);
end;

procedure TLTWorldObject.WritePropVector(szName: string; vData: LTVector);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(PT_VECTOR, szName, @vData);
  m_pPropertyList.Add(pProperty);
end;

procedure TLTWorldObject.WritePropColor(szName: string; vData: LTVector);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(PT_COLOR, szName, @vData);
  m_pPropertyList.Add(pProperty);
end;

procedure TLTWorldObject.WritePropReal(szName: string; fData: LTFloat);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(PT_REAL, szName, @fData);
  m_pPropertyList.Add(pProperty);
end;

procedure TLTWorldObject.WritePropLongInt(szName: string; dwData: Cardinal);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(PT_LONGINT, szName, @dwData);
  m_pPropertyList.Add(pProperty);
end;

procedure TLTWorldObject.WritePropFlags(szName: string; dwData: Cardinal);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(PT_FLAGS, szName, @dwData);
  m_pPropertyList.Add(pProperty);
end;

procedure TLTWorldObject.WritePropBool(szName: string; nData: Byte);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(PT_BOOL, szName, @nData);
  m_pPropertyList.Add(pProperty);
end;

procedure TLTWorldObject.WritePropRotation(szName: string; rData: LTRotation);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(PT_ROTATION, szName, @rData);
  m_pPropertyList.Add(pProperty);
end;

function TLTWorldObject.GetObjectName: string;
var i: Cardinal;
begin
  Result := '';
  if m_pPropertyList.Count > 0 then
  begin
    for i := 0 to m_pPropertyList.Count - 1 do
    begin
      if (TLTWorldObjectProperty(m_pPropertyList.Items[i]).PropName = 'Name') and
      (TLTWorldObjectProperty(m_pPropertyList.Items[i]).PropCode = PT_STRING) then
      Exit(TLTWorldObjectProperty(m_pPropertyList.Items[i]).DataStr);
    end;
  end;
end;

procedure TLTWorldObject.CreateProperty(nCode: Byte; szName: string;
  pData: Pointer);
var pProperty: TLTWorldObjectProperty;
begin
  Inc(m_nNumProperties, 1);
  pProperty := TLTWorldObjectProperty.Create;
  pProperty.WriteProperty(nCode, szName, pData);
  m_pPropertyList.Add(pProperty);
end;

procedure TLTWorldObject.ReadObject(FS: TMemoryStream);
var i: Cardinal;
    ObjProperty: TLTWorldObjectProperty;
begin
  FS.Read(m_nDataLength, 2);
  FS.Read(m_nTypeStringLen, 2);
  SetLength(m_szTypeString, m_nTypeStringLen);
  FS.Read(m_szTypeString[1], m_nTypeStringLen);
  FS.Read(m_nNumProperties, 4);
  if m_nNumProperties > 0 then
  begin
    for i := 0 to m_nNumProperties - 1 do
    begin
      ObjProperty := TLTWorldObjectProperty.Create;
      ObjProperty.ReadProperty(FS);
      m_pPropertyList.Add(ObjProperty);
    end;
  end;
end;

constructor TLTWorldObject.Create;
begin
  m_pPropertyList := TFPObjectList.Create(True);
end;

destructor TLTWorldObject.Destroy;
begin
  m_pPropertyList.Free;
  inherited;
end;

function TLTWorldObjectProperty.DataGeneric: string;
begin
  case m_nPropCode of
    PT_STRING: Result := m_szData;
    PT_VECTOR, PT_COLOR: Result := LTVectorToStrC(@m_vData);
    PT_REAL: Result := FormatFloat('0.000000', m_fData);
    PT_BOOL: Result := IntToStr(m_nData);
    PT_FLAGS, PT_LONGINT: Result := FormatFloat('0.000000', PLTFloat(@m_dwData)^);  //IntToHex(m_dwData, 8);
    PT_ROTATION: Result := LTRotationToStrC(@m_rData);
  end;
end;

procedure TLTWorldObjectProperty.ReadProperty(FS: TMemoryStream);
var n: Word;
begin
  n := 0;
  FS.Read(m_nNameLength, 2);
  SetLength(m_szName, m_nNameLength);
  FS.Read(m_szName[1], m_nNameLength);
  FS.Read(m_nPropCode, 1);
  FS.Read(m_nPropFlags, 4);
  FS.Read(m_nPropDataLength, 2);
  // data specific
  case m_nPropCode of
    PT_STRING:
      begin
        FS.Read(n, 2);
        SetLength(m_szData, n);
        if n > 0 then FS.Read(m_szData[1], n);
      end;
    PT_VECTOR, PT_COLOR:
      begin
        FS.Read(m_vData, sizeof(LTVector));
      end;
    PT_REAL:
      begin
        FS.Read(m_fData, 4);
      end;
    PT_BOOL:
      begin
        FS.Read(m_nData, 1);
      end;
    PT_FLAGS, PT_LONGINT:
      begin
        FS.Read(m_dwData, 4);
      end;
    PT_ROTATION:
      begin
        FS.Read(m_rData, sizeof(LTRotation));
      end;
  end;
end;

procedure TLTWorldObjectProperty.WriteProperty(nCode: Byte; szName: string;
  pData: Pointer);
begin
  m_nPropCode := nCode;
  m_szName := szName;
  case m_nPropCode of
    PT_STRING:
      begin
        m_szData := string(pData^);
      end;
    PT_VECTOR, PT_COLOR:
      begin
        m_vData := LTVector(pData^);
      end;
    PT_REAL:
      begin
        m_fData := LTFloat(pData^);
      end;
    PT_BOOL:
      begin
        m_nData := Byte(pData^);
      end;
    PT_FLAGS, PT_LONGINT:
      begin
        m_dwData := Integer(pData^);
      end;
    PT_ROTATION:
      begin
        m_rData := LTRotation(pData^);
      end;
  end;
end;

constructor TLTWorldObjectProperty.Create;
begin

end;

destructor TLTWorldObjectProperty.Destroy;
begin
  inherited;
end;


end.

