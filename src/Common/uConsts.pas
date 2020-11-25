unit uConsts;

interface

uses
  System.Generics.Collections, System.SysUtils, uClassMessageDTO;

type
  TTelegramButtons = TDictionary<String, String>;
  TProcErrorException = procedure(AExcept: Exception) of object;
  TChatMessageDTOList = TObjectList<TChatMessageDTO>;

implementation

end.
