type vALUE =
  | Int of int
  | Bool of bool
  | String of string
  | Name of string
  | Unit
  | Error

  | CLOSURE of (string  * (cOMMAND list) * (string * vALUE) list list )

and cOMMAND =
  | Push of vALUE
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg
  | Swap
  | ToString
  | Println
  | Quit

  | Cat 
  | And 
  | Or 
  | Not 
  | Equal 
  | LessThan 
  | If
  | Bind 
  | Let 
  | End

  | FunBind of string * string     
  | InOutFunBind of string * string  
  | FunEnd
  | Call
  | Return


let interpreter ((input :  string), (output :  string )) :  unit =


  let ic = open_in input in
  
  let oc = open_out output in

  let rec loop_read acc =

    try

      let l = String.trim(input_line ic) in

      loop_read (l :: acc)

    with End_of_file -> List.rev acc

  in

  

  let aLPHA = [
    'a'; 'b'; 'c'; 
    'd'; 'e'; 'f';
    'g'; 'h'; 'i'; 
    'j'; 'k'; 'l';
    'm'; 'n'; 'o'; 
    'p'; 'q'; 'r';
    's'; 't'; 'u'; 
    'v'; 'w'; 'x';
    'y'; 'z'; 'A'; 
    'B'; 'C'; 'D';
    'E'; 'F'; 'G'; 
    'H'; 'I'; 'J';
    'K'; 'L'; 'M'; 
    'N'; 'O'; 'P';
    'Q'; 'R'; 'S'; 
    'T'; 'U'; 'V';
    'W'; 'X'; 'Y'; 
    'Z'
  ] 
  in

  let nUM = [
    '0'; '1'; '2'; 
    '3'; '4'; '5'; 
    '6'; '7'; '8'; 
    '9'
  ]
  in 

  let rec eXISTS lIST cHARACTER =


    match lIST with

    | [] -> false
    | hEAD :: tAIL -> 

      if hEAD = cHARACTER 
        then true 
      else eXISTS tAIL cHARACTER

  in

  let iSNAME sTRING =

    let cHARACTERINNAME cHARACTER = eXISTS aLPHA cHARACTER 
                                    || eXISTS nUM cHARACTER 
                                    || cHARACTER = '_' in

    String.length sTRING > 0 &&
    (eXISTS aLPHA sTRING.[0] || sTRING.[0] = '_') && 
    String.for_all cHARACTERINNAME sTRING

  in

  let isINT sTRING =
    try Some (int_of_string sTRING)
    with Failure _ -> None
  in

  let nOTCHAR cHARACTER = (cHARACTER = '"') || (cHARACTER = '\\') in

  let mATCHVALUE sTRING =


    match sTRING with
    | ":true:" -> Bool true
    | ":false:" -> Bool false
    | ":unit:" -> Unit
    | ":error:" -> Error

    | _ ->
        if String.length sTRING >= 2 && sTRING.[0] = '"' && sTRING.[String.length sTRING - 1] = '"' then

          let sPLITS = String.split_on_char '"' sTRING in

          match sPLITS with

          | "" :: iNSIDE :: [""] ->

              if String.exists nOTCHAR iNSIDE then Error
              else String iNSIDE

          | _ -> Error



        else match isINT sTRING with

          | Some nUMBER -> Int nUMBER
          | None -> 

              if iSNAME sTRING then Name sTRING 
              else Error

  in




  let rec mAKESTRING lINE =

    match lINE with


    | [] -> ""
    | hEAD :: [] -> hEAD

    | hEAD :: tAIL -> hEAD ^ " " ^ mAKESTRING tAIL

  in  

  let mATCHCOMMAND lINE =


    let cOMMANDS = String.split_on_char ' ' lINE in

    match cOMMANDS with

    | "push" :: sTRING ->
      let vALUE = mAKESTRING sTRING 
      in 
      Push (mATCHVALUE vALUE)

    | ["pop"] -> Pop

    | ["add"] -> Add
    | ["sub"] -> Sub
    | ["mul"] -> Mul

    | ["div"] -> Div
    | ["rem"] -> Rem

    | ["neg"] -> Neg

    | ["swap"] -> Swap

    | ["toString"] -> ToString
    | ["println"] -> Println

    | ["quit"] -> Quit


    | ["cat"] -> Cat 
    | ["and"] -> And 
    | ["or"] -> Or

    | ["not"] -> Not 
    | ["equal"] -> Equal
    | ["lessThan"] -> LessThan 

    | ["bind"] -> Bind
    | ["if"] -> If 
    | ["let"] -> Let 

    | ["end"] -> End

    
    | ["call"] -> Call
    | ["return"] -> Return

    | "fun" :: fUNCTIONNAME :: fUNCTIONINPUT :: [] -> FunBind (fUNCTIONNAME, fUNCTIONINPUT)
    | "inOutFun" :: fUNCTIONNAME :: fUNCTIONINPUT :: [] -> InOutFunBind (fUNCTIONNAME, fUNCTIONINPUT)

    | ["funEnd"] -> FunEnd


    | _ -> Push Error
  
  in

  let sTRINGOFVALUE vALUE =

    match vALUE with


    | Int nUMBER -> string_of_int nUMBER

    | Bool true -> ":true:"
    | Bool false -> ":false:"

    | String sTRING -> sTRING
    | Name nAME -> nAME

    | Unit -> ":unit:"
    | Error -> ":error:"

    | CLOSURE _ -> ":fun:"

  in




  

  
  let rec aBINDINGEXISTS tHENAME bINDS =

    match bINDS with

    | [] -> None  
    | bIND :: rEMAINING ->


        match bIND with


        | [] -> aBINDINGEXISTS tHENAME rEMAINING

        | (nAME, vALUE) :: tAIL ->
            if nAME = tHENAME 
              then Some vALUE

            else aBINDINGEXISTS tHENAME (tAIL :: rEMAINING)
  
  in

  let nEWbIND nAME vALUE bINDS =


    match bINDS with

    | [] -> [(nAME, vALUE)] :: []

    | hEAD :: tAIL -> ((nAME, vALUE) :: hEAD) :: tAIL

  in

  let gETBIND vALUE bINDS =


    match vALUE with

    | Name nAME -> (


      match aBINDINGEXISTS nAME bINDS with 
      | Some vaLUE -> vaLUE
      | None -> Error

      )

    | _ -> vALUE 

  in  

  let rec cOMMANDS coMMANDS vaLUES bINDS =

    match (coMMANDS, vaLUES, bINDS) with


    | ([], valUES, _) -> valUES

    
       
    | (Push vALUE :: comMANDS, valUES, binDS) -> cOMMANDS comMANDS (vALUE :: valUES) binDS
  


    | (Pop :: comMANDS, [], binDS) -> cOMMANDS comMANDS [Error] binDS
    | (Pop :: comMANDS, _ :: valUES, binDS) -> cOMMANDS comMANDS valUES binDS


    | (Add :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
       | Int fIRST, Int sECOND -> cOMMANDS comMANDS (Int (sECOND + fIRST) :: valUES) binDS
       | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)

  
    | (Add :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Add :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS


    | (Sub :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
       | Int fIRST, Int sECOND -> cOMMANDS comMANDS (Int (sECOND - fIRST) :: valUES) binDS
       | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)
  
  
   
    | (Sub :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Sub :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS


    | (Mul :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
       | Int fIRST, Int sECOND -> cOMMANDS comMANDS (Int (sECOND * fIRST) :: valUES) binDS
       | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)
    
    
    | (Mul :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Mul :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS


    | (Div :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
      | Int 0, Int _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS
      | Int fIRST, Int sECOND -> cOMMANDS comMANDS (Int (sECOND / fIRST) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)
    
   
    | (Div :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Div :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS


    | (Rem :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
      | Int 0, Int _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS
      | Int fIRST, Int sECOND -> cOMMANDS comMANDS (Int (sECOND mod fIRST) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)

   
    | (Rem :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Rem :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS


    | (Neg :: comMANDS, fIRST :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      (match fIRSTVALUE with
      | Int fIRST -> cOMMANDS comMANDS (Int (-fIRST) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: valUES) binDS)
    
    
    | (Neg :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS


    | (Swap :: comMANDS, fIRST :: sECOND :: valUES, binDS) -> cOMMANDS comMANDS (sECOND :: fIRST :: valUES) binDS

    | (Swap :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Swap :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS


    | (ToString :: comMANDS, vALUE :: valUES, binDS) -> cOMMANDS comMANDS (String (sTRINGOFVALUE vALUE) :: valUES) binDS
    
    | (ToString :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS
  

    | (Println :: comMANDS, String sTRING :: valUES, binDS) -> (
        Printf.fprintf oc "%s\n" sTRING; 
        cOMMANDS comMANDS valUES binDS)
    | (Println :: comMANDS, vALUE :: valUES, binDS) -> cOMMANDS comMANDS (Error :: vALUE :: valUES) binDS

    | (Println :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS


    | (Quit :: _, valUES, _) -> valUES








    
    | (Cat :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
      | String fIRST, String sECOND -> cOMMANDS comMANDS (String (sECOND ^ fIRST) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)

    | (Cat :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Cat :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS



    | (And :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
      | Bool fIRST, Bool sECOND -> cOMMANDS comMANDS (Bool (fIRST && sECOND) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)
    
    | (And :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (And :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS



    | (Or :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
      | Bool fIRST, Bool sECOND -> cOMMANDS comMANDS (Bool (fIRST || sECOND) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)
    
    | (Or  :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Or  :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS



    | (Not :: comMANDS, fIRST :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      (match fIRSTVALUE with
      | Bool b -> cOMMANDS comMANDS (Bool (not b) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: valUES) binDS)

    | (Not :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS
    


    | (Equal :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
      | Int fIRST, Int sECOND -> cOMMANDS comMANDS (Bool (fIRST = sECOND) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)

    | (Equal :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Equal :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS



    | (LessThan :: comMANDS, fIRST :: sECOND :: valUES, binDS) ->
      let fIRSTVALUE = gETBIND fIRST binDS in
      let sECONDVALUE = gETBIND sECOND binDS in
      (match (fIRSTVALUE, sECONDVALUE) with
      | Int fIRST, Int sECOND -> cOMMANDS comMANDS (Bool (sECOND < fIRST) :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS)

    | (LessThan :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (LessThan :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS

    
    
    

    
    | (Bind :: comMANDS, vALUE :: Name nAME :: valUES, binDS) ->
      let biND = gETBIND vALUE binDS in
      (match biND with
        | Error -> cOMMANDS comMANDS (Error :: vALUE :: Name nAME :: valUES) binDS
        | _ -> cOMMANDS comMANDS (Unit :: valUES) (nEWbIND nAME biND binDS))
    
    | (Bind :: comMANDS, fIRST :: sECOND :: valUES, binDS) -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: valUES) binDS
    | (Bind :: comMANDS, fIRST :: [], binDS) -> cOMMANDS comMANDS (Error :: fIRST :: []) binDS
    | (Bind :: comMANDS, [], binDS) -> cOMMANDS comMANDS (Error :: []) binDS

   
  
  


    | (If :: comMANDS, fIRST :: sECOND :: cOND :: valUES, binDS) ->
      let cONDVALUE = gETBIND cOND binDS in
      (match cONDVALUE with
      | Bool true -> cOMMANDS comMANDS (fIRST :: valUES) binDS
      | Bool false -> cOMMANDS comMANDS (sECOND :: valUES) binDS
      | _ -> cOMMANDS comMANDS (Error :: fIRST :: sECOND :: cOND :: valUES) binDS)
    
    | (If :: comMANDS, _, binDS) -> cOMMANDS comMANDS (Error :: []) binDS

      


    | (Let :: comMANDS, vaLUES, bINDS) ->
      let rec sORT rEMANINGCOMMANDS nUMBEROFLETS nOWCOMMANDS =
        match rEMANINGCOMMANDS with
        | [] -> (List.rev nOWCOMMANDS, [])
        | End :: tAIL -> (
          match nUMBEROFLETS with
            | 0 -> (List.rev nOWCOMMANDS, tAIL)
            | _ -> sORT tAIL (nUMBEROFLETS - 1) (End :: nOWCOMMANDS)
          )
        | Let :: tAIL -> sORT tAIL (nUMBEROFLETS + 1) (Let :: nOWCOMMANDS)
        | hEAD :: tAIL -> sORT tAIL nUMBEROFLETS (hEAD :: nOWCOMMANDS)
      in
      let (cOMMANDSNOW, cOMMANDSREMAINING) = sORT comMANDS 0 [] in
      let valueS = cOMMANDS cOMMANDSNOW [] ([] :: bINDS) in
      (match valueS with
        | vALUE :: _ -> cOMMANDS cOMMANDSREMAINING (vALUE :: vaLUES) bINDS
        | [] -> cOMMANDS cOMMANDSREMAINING (Error :: vaLUES) bINDS)
      
    
    
        

    
    
    
        
       
    


    | (End :: comMANDS, fIRST :: _, _ :: bINDS) -> cOMMANDS comMANDS (fIRST :: vaLUES) bINDS
    
    | (End :: comMANDS, [], _ :: bINDS) -> cOMMANDS comMANDS (Error :: vaLUES) bINDS
    | (End :: comMANDS, fIRST :: valUES, []) -> cOMMANDS comMANDS (Error :: fIRST :: valUES) []
    | (End :: comMANDS, [], _) -> cOMMANDS comMANDS (Error :: []) bINDS

    


    
    
    | (FunBind(fUNCTIONNAME, fUNCTIONINPUT) :: oTHERCOMMANDS, valUES, bINDS) ->
      (let rec fUNCTIONIN commaNDS mAKE lEVEL =
        match commaNDS with
        | [] -> List.rev mAKE, []
        
        | FunBind (fuNCTIONNAME, fuNCTIONINPUT) :: tAIL ->
          fUNCTIONIN tAIL (FunBind (fuNCTIONNAME, fuNCTIONINPUT) :: mAKE) (lEVEL + 1)

        | InOutFunBind (fuNCTIONNAME, fuNCTIONINPUT) :: tAIL -> 
          fUNCTIONIN tAIL (InOutFunBind (fuNCTIONNAME, fuNCTIONINPUT) :: mAKE) (lEVEL + 1)


        
        | FunEnd :: tAIL -> (
          match lEVEL with
          | 0 -> (List.rev mAKE, tAIL)
          | iNTIGER -> fUNCTIONIN tAIL (FunEnd :: mAKE) (iNTIGER - 1))
        | hEAD :: tAIL -> fUNCTIONIN tAIL (hEAD :: mAKE) lEVEL

      in

      let nOWCOMMANDS, tAIL = fUNCTIONIN oTHERCOMMANDS [] 0 in
      
      let cLOSURE = CLOSURE(fUNCTIONINPUT, nOWCOMMANDS, [] :: bINDS) in
      let nOWBIND = nEWbIND fUNCTIONNAME cLOSURE bINDS in
      cOMMANDS tAIL (Unit :: valUES) nOWBIND)




    | (InOutFunBind(fUNCTIONNAME, fUNCTIONINPUT) :: oTHERCOMMANDS, valUES, bINDS) ->
      (let rec fUNCTIONIN commaNDS mAKE lEVEL =
        match commaNDS with
        | [] -> List.rev mAKE, []
        
        | FunBind (fuNCTIONNAME, fuNCTIONINPUT) :: tAIL ->
          fUNCTIONIN tAIL (FunBind (fuNCTIONNAME, fuNCTIONINPUT) :: mAKE) (lEVEL + 1)

        | InOutFunBind (fuNCTIONNAME, fuNCTIONINPUT) :: tAIL -> 
          fUNCTIONIN tAIL (InOutFunBind (fuNCTIONNAME, fuNCTIONINPUT) :: mAKE) (lEVEL + 1)


        
        | FunEnd :: tAIL -> (
          match lEVEL with
          | 0 -> (List.rev mAKE, tAIL)
          | iNTIGER -> fUNCTIONIN tAIL (FunEnd :: mAKE) (iNTIGER - 1))
        | hEAD :: tAIL -> fUNCTIONIN tAIL (hEAD :: mAKE) lEVEL

        in


        let nOWCOMMANDS, tAIL = fUNCTIONIN oTHERCOMMANDS [] 0 in


        let cLOSURE = CLOSURE(fUNCTIONINPUT, nOWCOMMANDS, ([ (fUNCTIONINPUT, Unit) ] :: bINDS)) in
        let nOWBIND = nEWbIND fUNCTIONNAME cLOSURE bINDS in
        cOMMANDS tAIL (Unit :: valUES) nOWBIND)

  
    
    | (FunEnd :: tAIL, valUES, binDS) -> (cOMMANDS tAIL (Error :: valUES) binDS)
  
    | (Call :: tAIL, fIRST :: sECOND :: valUES, binDS) ->

      (let sECONDVALUE = gETBIND sECOND binDS in
      let fIRSTVALUE = gETBIND fIRST binDS in

      (match sECONDVALUE with

      | CLOSURE(strING, vAlueS, nOWBINDS) ->

          

          let fLAG =
            match nOWBINDS with
            | ((sTRING, Unit) :: []) :: _ -> 
              if sTRING = strING then 
                true 
              else 
                false
            | _ -> false
          in
          
          let fUNCTIONBINDS = [ (strING, fIRSTVALUE) ] :: nOWBINDS in
          let fUNCTIONVALUES = cOMMANDS vAlueS [] fUNCTIONBINDS in

          let rETURN =
            match fUNCTIONVALUES with
            | valuE :: _ -> valuE
            | [] -> Error
          in

          let nOWBINDSS =
            if fLAG then
              match fIRST with
              | Name strinG ->
                  let nOWVALUE = gETBIND (Name strING) fUNCTIONBINDS in
                  nEWbIND strinG nOWVALUE binDS
              | _ ->
                binDS
            else
              binDS
          in
          cOMMANDS tAIL (rETURN :: valUES) nOWBINDSS
      | _ ->
          cOMMANDS tAIL (Error :: fIRST :: sECOND :: valUES) binDS)
      )
      

    | (Call :: tAIL, _, binDS) -> (cOMMANDS tAIL (Error :: []) binDS)

    
    | (Return :: _, valUES, _ )-> (valUES)
      
      
    

  in






  let rEADLINES = loop_read [] in

  let lINESTOCOMMANDS = List.map mATCHCOMMAND rEADLINES in
  let cOMMANDSTOVALUES = cOMMANDS lINESTOCOMMANDS [] [] in

  let oUTPUTLINES = List.map sTRINGOFVALUE (List.rev cOMMANDSTOVALUES) in

  List.iter (fun lINE -> Printf.fprintf oc "%s\n" lINE) oUTPUTLINES; 

