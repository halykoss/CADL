open Batteries

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

open FunSpecification.FunSpecification

let initial_gamma_list = [
]

let rec nodecount e = match e with
    Zero(annot)   -> 1
  | Output(_,_,e1,annot)
  | Input(_,_,e1,annot)
  | Skd(_,_,_,e1,annot) 
  | Match(_,_,e1,annot)
  | Splitting(_,_,_,e1,annot)
  | Restriction(_,_,e1,annot) 
  | Replication(e1,annot)	-> 1 + nodecount e1
  | IntCase(_,e1,_,e2,annot) 
  |  Parallel(e1,e2,annot) -> 1 + nodecount e1 + nodecount e2

let analyze_expr (file : string) (filem : string) =
    Printf.printf "Analyzing: Orig: %s ... Mod: %s ...\n" file filem;
    let channel, channelm = open_in file, open_in filem in
    let lexbuf, lexbufm = Lexing.from_channel channel, Lexing.from_channel channelm in
    let e, em = Parser.exp Lexer.token lexbuf, Parser.exp Lexer.token lexbufm in
    let e_hf, em_hf = (Id.counter := 0; OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) e), (Id.counter := 0; OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) em) in (* Id.counter := 0 to to avoid that the same subtree gets different hashes *)
    let gamma_init, gamma_initm = (FunContext.add_list (initial_gamma_list) (FunContext.get_empty_context ())), (FunContext.add_list (initial_gamma_list) (FunContext.get_empty_context ())) in
    (* Printf.printf "Program: %s\n" (FunSpecification.FunSpecification.string_of_term (fun f x -> ()) e);
    Printf.printf "Program Mod: %s\n" (FunSpecification.FunSpecification.string_of_term (fun f x -> ()) em); *)
    let cache = IncrementalFunAlgorithm.get_empty_cache 4096 in
    ignore (IncrementalFunAlgorithm.build_cache e_hf gamma_init cache);
    IncrementalFunAlgorithm.IncrementalReport.reset IncrementalFunAlgorithm.report;
    IncrementalFunAlgorithm.IncrementalReport.set_nc (nodecount em_hf) IncrementalFunAlgorithm.report;
    let te, tem = OriginalFunAlgorithm.typing gamma_init e_hf, IncrementalFunAlgorithm.typing cache gamma_initm em_hf in
        Printf.printf "Type: %s - IType: %s\n" (FunSpecification.FunSpecification.string_of_type te) (FunSpecification.FunSpecification.string_of_type tem);
        Printf.printf "%s\n" (IncrementalFunAlgorithm.IncrementalReport.string_of_report IncrementalFunAlgorithm.report)


let _ =
    if Array.length Sys.argv = 3 then
        begin
            analyze_expr Sys.argv.(1) Sys.argv.(2);
            exit 0
        end
    else
        Printf.printf "Usage:\n %s file1.ml file2.ml\n" Sys.argv.(0)
