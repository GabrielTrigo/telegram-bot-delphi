unit uClassMessageDTO;

interface

uses
  REST.Json.Types;

{$M+}

type
  TChatDTO = class
  private
    FFirst_Name: string;
    FId: Integer;
    FLast_Name: string;
    FType: string;
    FUsername: string;
  published
    property First_Name: string read FFirst_Name write FFirst_Name;
    property Id: Integer read FId write FId;
    property Last_Name: string read FLast_Name write FLast_Name;
    property &Type: string read FType write FType;
    property Username: string read FUsername write FUsername;
  end;

  TFromDTO = class
  private
    FFirst_Name: string;
    FId: Integer;
    FIs_Bot: Boolean;
    FLanguage_Code: string;
    FLast_Name: string;
    FUsername: string;
  published
    property First_Name: string read FFirst_Name write FFirst_Name;
    property Id: Integer read FId write FId;
    property Is_Bot: Boolean read FIs_Bot write FIs_Bot;
    property Language_Code: string read FLanguage_Code write FLanguage_Code;
    property Last_Name: string read FLast_Name write FLast_Name;
    property Username: string read FUsername write FUsername;
  end;

  TMessageDTO = class
  private
    FChat: TChatDTO;
    FDate: Integer;
    FFrom: TFromDTO;
    FMessage_Id: Integer;
    FText: string;
  published
    property Chat: TChatDTO read FChat write FChat;
    property Date: Integer read FDate write FDate;
    property From: TFromDTO read FFrom write FFrom;
    property Message_Id: Integer read FMessage_Id write FMessage_Id;
    property Text: string read FText write FText;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChatMessageDTO = class
  private
    [JSONNameAttribute('message')]
    FMessage: TMessageDTO;
    FUpdate_Id: Integer;
  published
    property Message: TMessageDTO read FMessage write FMessage;
    property Update_Id: Integer read FUpdate_Id write FUpdate_Id;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMessageDTO }

constructor TMessageDTO.Create;
begin
  inherited;
  FFrom := TFromDTO.Create;
  FChat := TChatDTO.Create;
end;

destructor TMessageDTO.Destroy;
begin
  FFrom.Free;
  FChat.Free;
  inherited;
end;

{ TChatMessageDTO }

constructor TChatMessageDTO.Create;
begin
  FMessage := TMessageDTO.Create;
end;

destructor TChatMessageDTO.Destroy;
begin
  FMessage.Free;
  inherited;
end;

end.
