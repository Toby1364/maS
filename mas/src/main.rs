use std::fs;
use std::env;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Range;
use std::path::Path;

use colored::Colorize;
use multipeek::multipeek;

fn format_radix(mut x: u32, radix: u32) -> String {
    let mut result = vec![];

    loop {
        let m = x % radix;
        x = x / radix;

        // will panic if you use a bad radix (< 2 or > 36).
        result.push(std::char::from_digit(m, radix).unwrap());
        if x == 0 {
            break;
        }
    }
    result.into_iter().rev().collect()
}


#[derive(Clone, Debug, PartialEq)]
enum Symbol {
    Custom(String),

    Lua,

    Include,
    Namespace,
    Using,
    Func,
    Return,

    Loop,
    While,
    Do,
    For,
    In,
    Break,
    Goto,
    Continue,

    If,
    Else,

    Let,
    Mut,

    Str,
    Num,
    Bool,
    Any,

    Array(Box<Symbol>),

    Or,
    And,

    Not,
    Hash,

    Equal,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Power,

    Less,
    Greater,

    Dot,
    Comma,
    Colon,
    Semicolon,

    RoundOpen,
    RoundClose,

    SquareOpen,
    SquareClose,

    CurlyOpen,
    CurlyClose,
}

#[derive(Clone, Debug)]
struct Token {
    symbol: Symbol,
    file: String,
    line_number: usize
}

impl PartialEq for Token {
    fn eq(&self, rhs: &Token) -> bool {
        self.symbol == rhs.symbol
    }
}

impl Token {
    fn new(symbol: Symbol, file: String, line_number: usize) -> Self {
        Self {
            symbol,
            file,
            line_number,
        }
    }
    fn sym(symbol: Symbol) -> Self {
        Self {
            symbol,
            file: String::from(""),
            line_number: 0,
        }
    }
}

fn main() {
    let args = env::args().collect::<Vec<String>>();

    let mut flags = HashSet::new();

    for arg in &args {
        if arg.starts_with("--") { flags.insert(arg.strip_prefix("--").unwrap().to_owned()); }
    }

    if args.len() < 2 {
        println!("\n{}\n{} {} {} {}\n\n{}\n{}{}\n{}{}\n", 
            "Usage:".bold().green(), 
            "    maS".bold().cyan(), "<PATH>".cyan(), "[FLAGS]".cyan(), "\tTranspiles the provided file.",

            "Flags:".bold().green(),
            "    --readable".bold().cyan(), "\tKeeps original and internal varaible names intact.",
            "    --keep    ".bold().cyan(), "\tKeeps unused functions and variables."
        );
    }
    else if let Some(mut tokens) = tokenize(args[1].clone()) {
        let mut indexer = 0;

        //for t in &tokens { println!("{:?}", t.symbol) }

        let mut vars: HashMap<usize,HashMap<String, (Symbol, bool)>> = HashMap::new();
        let mut last_func_name = String::new();

        let mut dos: Vec<usize> = Vec::new();

        let mut namespace: Vec<String> = Vec::new();
        let mut using: Vec<String> = Vec::new();

        let mut expect_scope = false;

        let mut scope = 0;
        let mut lua = String::new();

        lua.push_str("local function ___internal_application_launcher() _G.main() end ");

        let mut i = 0;
        while i < tokens.len() {
            //println!("{}|{}: {:?}", scope, tokens[i].line_number, tokens[i].symbol);

            let ns = namespace.iter().map(|s| s.clone()).collect::<Vec<String>>();
            let mut ns = ns.join("__"); if !ns.is_empty() { ns.push_str("__") }

            //println!("{}", ns);

            match &tokens[i].symbol {
                Symbol::Custom(s) => {
                    //println!("{:?}", s);
                    if !s.is_empty() {
                        if s == " " { lua.push(' '); i += 1; continue }

                        if tokens[i+1].symbol == Symbol::Colon && tokens[i+2].symbol != Symbol::Colon {
                            lua.push_str(&format!("::{}::", s));
                            i += 2; continue
                        }

                        let oi = i;

                        let mut exp = Vec::new();
                        while tokens[i].symbol != Symbol::CurlyOpen && tokens[i].symbol != Symbol::CurlyClose && tokens[i].symbol != Symbol::Semicolon { exp.push(tokens[i].clone()); i += 1; }
                        i -= 1;
                            
                        //println!("{:?}", exp.iter().map(|t| t.symbol.clone()).collect::<Vec<Symbol>>());  

                        if tokens[i+1].symbol == Symbol::Semicolon {   
                            if exp.contains(&Token::sym(Symbol::Equal)) {
                                let exps = exp.split(|t| *t == Token::sym(Symbol::Equal)).collect::<Vec<&[Token]>>();

                                let mut exp_d = exps[0].to_vec();
                                let exp_s = exps[1].to_vec();

                                let mut operator: Option<char> = None;

                                match exps[0].last().unwrap().symbol {
                                    Symbol::Plus =>     { exp_d.pop(); operator = Some('+') }
                                    Symbol::Minus =>    { exp_d.pop(); operator = Some('-') }
                                    Symbol::Multiply => { exp_d.pop(); operator = Some('*') }
                                    Symbol::Divide =>   { exp_d.pop(); operator = Some('/') }
                                    Symbol::Modulo =>   { exp_d.pop(); operator = Some('%') }
                                    
                                    _ => {}
                                }

                                let mut eval_d = eval_exp(exp_d);
                                let eval_s = eval_exp(exp_s);
                                
                                let e_type = get_expr_type(&mut eval_d, &vars, scope, &ns, &using, &tokens[oi]);
                                
                                let dest = eval_str(eval_d, &e_type, &vars, scope, &ns, &using, &tokens[oi]);
                                let src = eval_str(eval_s, &e_type, &vars, scope, &ns, &using, &tokens[oi]);

                                if let Some(op) = operator {
                                    lua.push_str(&format!("{} = {} {op} {} ", dest, dest, src));
                                }
                                else {
                                    lua.push_str(&format!("{} = {} ", dest, src));
                                }
                            }
                            else {
                                let exp_eval = eval_str(eval_exp(exp), &Symbol::Any, &vars, scope, &ns, &using, &tokens[oi]);
                                if !exp_eval.is_empty() { lua.push_str(&format!("{exp_eval} ")) }
                                else { lua.push_str(&format!("{s} ")) }
                            }
                        }
                    }
                }

                Symbol::Lua => {
                    i += 2;
                    if tokens[i-1].symbol != Symbol::CurlyOpen {
                        let ln = &tokens[i-1].line_number.to_string(); 
                        if !ln.contains("1") && ln.ends_with("2") { 
                            error(&format!("Roses are red,\nViolets are blue\nMissing bracket\non line {}.\t", ln), &tokens[i-2]);
                        } 
                        else { 
                            error("Missing '{'", &tokens[i-2]);
                        } 
                    }

                    lua.push_str("if true then ");

                    let mut brackets = 1;

                    while brackets != 0 {
                        if tokens[i].symbol == Symbol::CurlyOpen { brackets += 1 }
                        else if tokens[i].symbol == Symbol::CurlyClose { brackets -= 1 }
                        if brackets == 0 { break }
                        lua.push_str(&sym_to_lua(&tokens[i].symbol));

                        i += 1;
                    }
                    lua.push_str("end ");
                }

                Symbol::Include => {
                    i += 1;

                    let mut exp = Vec::new();
                    while tokens[i].symbol != Symbol::CurlyOpen && tokens[i].symbol != Symbol::CurlyClose && tokens[i].symbol != Symbol::Semicolon { exp.push(tokens[i].clone()); i += 1; }

                    let mut path = Path::new(&tokens[i].file).to_owned();
                    path.pop();

                    path.push(eval_exp(exp).replace("__", "\\"));

                    if path.is_dir() { path.push("lib.mas") }
                    else { path.set_extension("mas"); }

                    if path.exists() {
                        if let Some(in_tokens) = tokenize(path.to_str().unwrap().to_owned()) {
                            let mut off_set = 0;
                            for tok in in_tokens {
                                tokens.insert(i+off_set, tok);
                                off_set += 1;
                            }
                        }
                        else { error(&format!("Unable to open file `{}`", path.to_str().unwrap()), &tokens[i-1]) }
                    }
                    else { error(&format!("Unable to locate file `{}`", path.to_str().unwrap()), &tokens[i-1]) }

                    i -= 1;

                    //println!("{:?}", tokens.iter().map(|t| t.symbol.clone()).collect::<Vec<Symbol>>());  
                }
                Symbol::Namespace => {
                    let name = match tokens[i+1].symbol.clone() {
                        Symbol::Custom(s) => s,
                        _ => { error("Invalid namespace name", &tokens[i+1]); String::from("") }
                    };

                    if tokens[i+2].symbol != Symbol::CurlyOpen {
                        let ln = &tokens[i].line_number.to_string(); 
                        if !ln.contains("1") && ln.ends_with("2") { 
                            error(&format!("Roses are red,\nViolets are blue\nMissing bracket\non line {}.\t", ln), &tokens[i]);
                        } 
                        else { 
                            error("Missing '{'", &tokens[i]);
                        } 
                    }

                    i += 2;

                    namespace.push(name);
                }
                Symbol::Using => {
                    i += 1;

                    let mut exp = Vec::new();
                    while tokens[i].symbol != Symbol::CurlyOpen && tokens[i].symbol != Symbol::CurlyClose && tokens[i].symbol != Symbol::Semicolon { exp.push(tokens[i].clone()); i += 1; }

                    let mut ns = String::new();

                    ns.push_str(&eval_exp(exp));

                    using.push(ns);
                }
                Symbol::Func => {
                    expect_scope = true;
                    let name = match tokens[i+1].symbol.clone() {
                        Symbol::Custom(s) => s,
                        _ => { error("Invalid function name", &tokens[i+1]); String::from("") }
                    };
                    last_func_name = name.clone();

                    let mut j = i;
                    while tokens[j+3].symbol != Symbol::CurlyOpen { j += 1; }

                    if !vars.contains_key(&scope) { vars.insert(scope, HashMap::new()); }
                    if tokens[j].symbol == Symbol::Minus {
                        tokens.remove(j);
                        tokens.remove(j);

                        vars.get_mut(&scope).unwrap().insert(format!("{}{}", ns, name), (tokens[j].symbol.clone(), false));
                    }
                    else {
                        vars.get_mut(&scope).unwrap().insert(format!("{}{}", ns, name), (Symbol::Any, false));
                    }

                    scope += 1;
                    j = i+3;

                    let mut args = Vec::new();
                    while tokens[j].symbol != Symbol::Semicolon && tokens[j].symbol != Symbol::RoundClose { args.push(tokens[j].clone()); j += 1; }

                    //println!("{:?}", args.iter().map(|t| t.symbol.clone()).collect::<Vec<Symbol>>());

                    let mut args_s = String::new();

                    for arg in &args {
                        match &arg.symbol {
                            Symbol::Custom(s) => args_s.push_str(&format!("{}{}", ns, s)),
                            Symbol::Comma => args_s.push_str(","),

                            sym => if let Some(s) = sym_to_str(&sym) { args_s.push_str(&format!("--[[{}]]", s)) },
                        }
                    }

                    //println!("{}", args_s);
                    //println!("{:?}", args);

                    let mut k = 0;
                    while k < args.len() {
                        let name = match &args[k].symbol {
                            Symbol::Custom(s) => s,
                            _ => { error("Invalid argument name", &args[k]); "" }
                        };
                        if !vars.contains_key(&scope) { vars.insert(scope, HashMap::new()); }
                        vars.get_mut(&scope).unwrap().insert(format!("{}{}", ns, name), (args[k+2].symbol.clone(), false));

                        k += 4;
                    }

                    scope -= 1;
                    i = j;

                    lua.push_str(&format!("local function {}({}) ", format!("{}{}", ns, name), args_s));
                }
                Symbol::Return => {
                    let oi = i;
                    i += 1;

                    let mut expresion = Vec::new();
                    while tokens[i].symbol != Symbol::Semicolon && tokens[i].symbol != Symbol::CurlyClose { expresion.push(tokens[i].clone()); i += 1; }
                    if tokens[i].symbol == Symbol::CurlyClose { i -= 1 }

                    lua.push_str(&format!("return {} ", eval_str(eval_exp(expresion), &vars.get(&0).unwrap().get(&format!("{}{}", ns, last_func_name)).unwrap().0, &vars, scope, &ns, &using, &tokens[oi])));
                }

                Symbol::Loop => {
                    expect_scope = true;
                    if tokens[i+1].symbol != Symbol::CurlyOpen {
                        let ln = &tokens[i-1].line_number.to_string(); 
                        if ln.ends_with("2") && !ln.contains("1") { 
                            error(&format!("Roses are red,\nViolets are blue\nMissing bracket\non line {}.\t", ln), &tokens[i]);
                        } 
                        else { 
                            error("Missing '{'", &tokens[i]);
                        } 
                    }
                    lua.push_str("while true do ");
                }
                Symbol::While => {
                    expect_scope = true;
                    i += 1;

                    let mut expresion = Vec::new();
                    while tokens[i].symbol != Symbol::CurlyOpen && tokens[i].symbol != Symbol::Semicolon { expresion.push(tokens[i].clone()); i += 1; }
                    i -= 1;

                    let exp = eval_str(eval_exp(expresion), &Symbol::Bool, &vars, scope, &ns, &using, &tokens[i]);

                    if dos.contains(&scope) {
                        lua = lua.strip_suffix("end ").unwrap().to_owned();
                        lua.push_str(&format!("until not ({}) ", exp));

                        if scope != dos.pop().unwrap() {
                            println!("I fucked up do while implementation")
                        }
                    }
                    else {
                        lua.push_str(&format!("while {} do ", exp));    
                    }
                }
                Symbol::Do => { expect_scope = true; lua.push_str("repeat "); dos.push(scope) }
                Symbol::For => {
                    expect_scope = true;
                    let oi = i;
                    i += 1;

                    let mut expresion = Vec::new();
                    while tokens[i].symbol != Symbol::CurlyOpen { expresion.push(tokens[i].clone()); i += 1; }
                    let exp = eval_exp(expresion);
                    i -= 1;

                    if exp.contains("§") {
                        let part = exp.split("§").map(|s| s.to_owned()).collect::<Vec<String>>();

                        let mut table = eval_str(part[1].to_owned(),&Symbol::Array(Box::new(Symbol::Any)), &vars, scope, &ns, &using, &tokens[oi]);

                        let table_type = get_expr_type(&mut table, &vars, scope, &ns, &using, &tokens[oi]);

                        lua.push_str(&format!("local for_loop_table_{indexer} = {table} for for_loop_i_{indexer}=1, #for_loop_table_{indexer} do local {} = for_loop_table_{indexer}[for_loop_i_{indexer}] ", part[0]));
                        indexer += 1;

                        match table_type {
                            Symbol::Array(t) => {
                                if !vars.contains_key(&scope) { vars.insert(scope, HashMap::new()); }
                                vars.get_mut(&scope).unwrap().insert(part[0].clone(), (*t, false));
                            }
                            _ => {}
                        }
                    }
                    else {
                        println!("For loop implementation missing.");
                    }
                }
                Symbol::Break => { lua.push_str("break ") }
                Symbol::Goto => {
                    i += 1;
                    lua.push_str(&format!("goto {} ", match &tokens[i].symbol {
                        Symbol::Custom(s) => s,
                        _ => { error("Invalid label", &tokens[i-1]); "" }
                    }));
                }
                Symbol::Continue => { lua.push_str("continue") }

                Symbol::If => {
                    expect_scope = true;
                    if lua.ends_with("else ") { lua = lua.strip_suffix(" ").unwrap().to_owned() }
                    i += 1;

                    let mut expresion = Vec::new();
                    while tokens[i].symbol != Symbol::CurlyOpen { expresion.push(tokens[i].clone()); i += 1; }
                    i -= 1;

                    lua.push_str(&format!("if {} then ", eval_str(eval_exp(expresion), &Symbol::Bool, &vars, scope, &ns, &using, &tokens[i])));
                }
                Symbol::Else => {
                    expect_scope = true;
                    lua = lua.strip_suffix("end ").unwrap().to_owned();
                    lua.push_str("else ") ;
                }

                Symbol::Let => {
                    let oi = i;

                    if tokens[i+1].symbol == Symbol::Mut {
                        let name = match &tokens[i+2].symbol {
                            Symbol::Custom(s) => s,
                            _ => { error("Invalid variable name", &tokens[i+2]); "" }
                        };
                        if let Some(v_type) = sym_to_str(&tokens[i+4].symbol) {

                            if !vars.contains_key(&scope) { vars.insert(scope, HashMap::new()); }
                            vars.get_mut(&scope).unwrap().insert(format!("{}{}", ns, name), (tokens[i+4].symbol.clone(), true));

                            i += 6;

                            let mut expresion = Vec::new();
                            let mut assigned = true;
                            if tokens[i-1].symbol != Symbol::Equal { expresion.push(Token::sym(Symbol::Custom(String::from("nil")))); assigned = false; i -= 1 }
                            while tokens[i].symbol != Symbol::Semicolon && tokens[i].symbol != Symbol::CurlyClose { if assigned { expresion.push(tokens[i].clone()) } i += 1; }

                            lua.push_str(&format!("local {} --[[{}]] = {} ", format!("{}{}", ns, name), v_type, eval_str(eval_exp(expresion), &tokens[oi+4].symbol, &vars, scope, &ns, &using, &tokens[oi])));
                        }
                        else { error(&format!("Unknown type `{:?}`", &tokens[i+4].symbol), &tokens[i+4]) }
                    }
                    else {
                        let name = match &tokens[i+1].symbol {
                            Symbol::Custom(s) => s,
                            _ => { error("Invalid variable name", &tokens[i+1]); "" }
                        };
                        if let Some(v_type) = sym_to_str(&tokens[i+3].symbol) {

                            if !vars.contains_key(&scope) { vars.insert(scope, HashMap::new()); }
                            vars.get_mut(&scope).unwrap().insert(format!("{}{}", ns, name), (tokens[i+3].symbol.clone(), false));
                            
                            i += 5;

                            let mut expresion = Vec::new();
                            let mut assigned = true;
                            if tokens[i-1].symbol != Symbol::Equal { expresion.push(Token::sym(Symbol::Custom(String::from("nil")))); assigned = false; i -= 1 }
                            while tokens[i].symbol != Symbol::Semicolon && tokens[i].symbol != Symbol::CurlyClose { if assigned { expresion.push(tokens[i].clone()) } i += 1; }
                            
                            lua.push_str(&format!("local {} --[[const {}]] = {} ", format!("{}{}", ns, name), v_type, eval_str(eval_exp(expresion), &tokens[oi+3].symbol, &vars, scope, &ns, &using, &tokens[oi])));
                        }
                        else { error(&format!("Unknown type `{:?}`", &tokens[i+3].symbol), &tokens[i+3]) }
                    }
                }

                Symbol::Str => { lua.push_str("--[[Str]]") }
                Symbol::Num => { lua.push_str("--[[Num]]") }
                Symbol::Bool => { lua.push_str("--[[Bool]]") }
                Symbol::Any => { lua.push_str("--[[Any]]") }
                Symbol::Array(t) => { lua.push_str( &if **t == Symbol::Any { String::from("--[[Table]]") } else { format!("--[[Array<{}>]]", sym_to_str(t).unwrap()) }) }

                Symbol::Comma => { lua.push_str(", ") }
                Symbol::Colon => {}
                Symbol::Semicolon => {}

                Symbol::RoundOpen => { lua.push('(') }
                Symbol::RoundClose => { lua.push(')') }

                Symbol::CurlyOpen => { 
                    scope += 1;
                    if !expect_scope {
                        lua.push_str("if true then ");
                    }
                    expect_scope = false;
                }
                Symbol::CurlyClose => { if scope == 0 { namespace.pop(); } else { lua.push_str("end "); vars.remove(&scope); scope -= 1 } }

                m => { println!("Missing handle for: {:?}", m) }
            }

            i += 1;
        }
        //println!("{}", scope as i64);

        //println!("{:?}", vars);

        lua.push_str("_G.main = main ___internal_application_launcher()");

        lua = postproces(lua, flags);

        fs::write("startup.lua", lua).expect("Couldn't write to startup.lua");

        println!("\n{}\n", "Finished transpiling".bold().green())
    }
    else {
        println!("\nUnable to read `{}`\n", args[1]);
    }
}

fn postproces(mut lua: String, flags: HashSet<String>) -> String {
    let mut creadits = String::from("--[[\n\nTranspiled using the original maS transpiler made by Toby1364\n\n]]\n");

    lua = lua.replace("001", "1");
    lua = lua.replace("--[[]]", " ");
    for _ in 0..10 { lua = lua.replace("  ", " ") }

    if !flags.contains("readable") {
        lua = remove_comments(lua);
    }

    if !flags.contains("keep") {
        for _ in 0..50 {
            let funcs = get_lua_funcs(lua.clone());
            for f in funcs {
                if !is_lua_func_called(lua.clone(), f.clone()) {
                    lua = remove_lua_func(lua, f);
                }
            }
        }

        lua = remove_unused_vars(lua);
    }

    if !flags.contains("readable") {
        lua = replace_var_names(lua);
    }

    lua = lua.replace("-1+1", "");

    creadits.push_str(&lua);
    return creadits;
}

fn get_lua_funcs(lua: String) -> Vec<String> {
    let mut functions: Vec<String> = Vec::new();

    let tokens = lua.split(|c: char| " +-*/%()<>~=,[]\n\r".contains(c)).map(|s| s.to_owned()).collect::<Vec<String>>();

    let mut i = 2;
    while i < tokens.len() {

        if tokens[i-2] == "local" && tokens[i-1] == "function" { functions.push(tokens[i].clone()); }

        i += 1;
    }

    return functions;
}

fn is_lua_func_called(lua: String, name: String) -> bool {
    let tokens = lua.split(|c: char| " +-*/%()<>~=,[]\n\r".contains(c)).map(|s| s.to_owned()).collect::<Vec<String>>();

    let mut i = 1;
    while i < tokens.len() {

        if tokens[i-1] != "function" && tokens[i] == name { return true }

        i += 1;
    }

    return false;
}

fn remove_lua_func(mut lua: String, name: String) -> String {
    let mut func = String::new();

    if let Some(index) = lua.find(&format!("local function {name}(")) {
        let mut scope = 1;

        while scope != 0 {
            func.push(lua.remove(index));

            if func.ends_with("]if ") || func.ends_with("\nif ") || func.ends_with(" if ") || func.ends_with(" for ") || func.ends_with(" while ") { scope += 1 }
            if func.ends_with(" end ") { scope -= 1 }
        }
    }

    return lua;
}

fn remove_unused_vars(mut lua: String) -> String {
    let tokens = lua.split(|c: char| " +-*/%()<>~=,[]\n\r".contains(c)).map(|s| s.to_owned()).collect::<Vec<String>>();

    let mut vars: HashSet<String> = HashSet::new();

    for i in 1..tokens.len() {
        match tokens[i].as_str() {
            "and" | "break" | "do" | "else" | "elseif" | "end" | "false" | "for" | 
            "function" | "if" | "in" | "local" | "nil" | "not" | "or" | "repeat" | 
            "return" | "then" | "true" | "until" | "while" => {}

            name => { if tokens[i-1] == "local" { vars.insert(name.to_owned()); } }
        }
    }

    let chars = lua.chars().collect::<Vec<char>>();

    let mut unused_vars = Vec::new();

    for var in vars {
        if lua.matches(&var).count() < 2 {
            let mut var_dec = String::new();

            let mut last_tok = String::new();

            if let Some(mut index) = lua.find(&format!("local {var}")) {

                while var_dec.len() <= 5 || match last_tok.as_str() {
                        "and" | "break" | "do" | "else" | "elseif" | "end" | "false" | "for" | 
                        "function" | "if" | "in" | "local" | "nil" | "not" | "or" | "repeat" | 
                        "return" | "then" | "true" | "until" | "while" => {false} _ => {true}} {
                    
                    if chars[index] == ' ' { last_tok.clear() }
                    else { last_tok.push(chars[index]) }
                    var_dec.push(chars[index]);

                    index += 1;
                }
            }

            while !var_dec.ends_with(' ') { var_dec.pop(); if var_dec.len() == 0 { println!("Fuck"); break } }

            unused_vars.push(var_dec);
        }
    }

    for var in unused_vars {
        //println!("{var}");
        lua = lua.replace(&var, "");
    }

    return lua;
}

fn replace_var_names(mut lua: String) -> String {
    let mut tokens = lua.split(|c: char| " +-*/%()<>~=,[]\n\r".contains(c)).map(|s| s.to_owned()).collect::<Vec<String>>();

    let mut vars: HashSet<String> = HashSet::new();

    let mut indexer = 0;

    let mut str = false;

    for i in 1..tokens.len() {
        if tokens[i] == " " || tokens[i].is_empty() { continue }
        if tokens[i].starts_with('\"') && !tokens[i].ends_with('\"') { str = true; continue }
        if str { if tokens[i].ends_with('\"') { str = false } continue }
        if tokens[i].parse::<f64>().is_ok() { continue }
        if let Some(n) = tokens[i].strip_prefix("0x") { if u64::from_str_radix(n, 16).is_ok() { continue } }
        if tokens[i].contains(|c: char| !c.is_alphanumeric() && c != '_') { continue; }

        match tokens[i].as_str() {
            "and" | "break" | "do" | "else" | "elseif" | "end" | "false" | "for" | 
            "function" | "if" | "in" | "local" | "nil" | "not" | "or" | "repeat" | 
            "return" | "then" | "true" | "until" | "while" => {}

            name => {
                if !vars.contains(name) {
                    vars.insert(name.to_owned());
                    while format_radix(indexer, 36).starts_with(|c: char| c.is_digit(10)) { indexer += 1 }


                    let rep = format_radix(indexer, 36);
                    if rep.len() < name.len() {
                        lua = replace_isolated(&lua, &name, &rep);
                        indexer += 1;
                    }

                    tokens = lua.split(|c: char| " +-*/%()<>~=,[]\n\r".contains(c)).map(|s| s.to_owned()).collect::<Vec<String>>();
                }
            }
        }
    }

    return lua;
}

fn remove_comments(lua: String) -> String {
    let mut stripped = String::new();

    let chars = lua.chars().collect::<Vec<char>>();

    let mut comment = false;

    for i in 0..chars.len() {
        if i > 0 && chars[i-1] == ']' && chars[i] == ']' { comment = false; continue }
        if i < chars.len() - 3 && chars[i] == '-' && chars[i+1] == '-' && chars[i+2] == '[' && chars[i+3] == '[' { comment = true }

        if !comment { stripped.push(chars[i]) }
    }

    for _ in 0..10 { stripped = stripped.replace("  ", " ") }

    return stripped;
}

fn replace_isolated(input: &str, from: &str, to: &str) -> String {
    let mut result = String::with_capacity(input.len());
    let chars: Vec<char> = input.chars().collect();
    let from_chars: Vec<char> = from.chars().collect();
    let from_len = from_chars.len();

    let mut string = false;

    let mut i = 0;
    while i <= chars.len() - from_len {
        if chars[i] != '\\' && chars[i+1] == '"' { string = !string }

        if &chars[i..i + from_len] == from_chars.as_slice() {
            let before = if i == 0 { None } else { Some(chars[i - 1]) };
            let after = chars.get(i + from_len).copied();

            let is_non_alnum = |c: Option<char>| {
                c.map_or(true, |c| !c.is_alphanumeric() && c != '_')
            };

            if !string && is_non_alnum(before) && is_non_alnum(after) {
                result.push_str(to);
                i += from_len;
                continue;
            }
        }

        result.push(chars[i]);
        i += 1;
    }

    // Push any trailing characters
    while i < chars.len() {
        result.push(chars[i]);
        i += 1;
    }

    result
}

fn error(msg: &str, token: &Token) {
    print!("\n{}", "Error: ".bold().red());
    println!("{} at {}", msg, format!("{}:{}", token.file, token.line_number).cyan());
}

fn sym_to_lua(sym: &Symbol) -> String {
    match sym {
        Symbol::Custom(s) => String::from(s),
        Symbol::Lua => String::from("lua"),
        Symbol::Include => String::from("include"),
        Symbol::Namespace => String::from("namespace"),
        Symbol::Using => String::from("using"),
        Symbol::Func => String::from("func"),
        Symbol::Return => String::from("return"),
        Symbol::Loop => String::from("loop"),
        Symbol::While => String::from("while"),
        Symbol::Do => String::from("do"),
        Symbol::For => String::from("for"),
        Symbol::In => String::from("in"),
        Symbol::Break => String::from("break"),
        Symbol::Goto => String::from("goto"),
        Symbol::Continue => String::from("continue"),
        Symbol::If => String::from("if"),
        Symbol::Else => String::from("else"),
        Symbol::Let => String::from("let"),
        Symbol::Mut => String::from("mut"),
        Symbol::Str => String::from("str"),
        Symbol::Num => String::from("num"),
        Symbol::Bool => String::from("bool"),
        Symbol::Any => String::from("any"),
        Symbol::Array(_) => String::from("array"),
        Symbol::Or => String::from("or"),
        Symbol::And => String::from("and"),
        Symbol::Not => String::from("not"),
        Symbol::Hash => String::from("#"),
        Symbol::Equal => String::from("="),
        Symbol::Plus => String::from("+"),
        Symbol::Minus => String::from("-"),
        Symbol::Multiply => String::from("*"),
        Symbol::Divide => String::from("/"),
        Symbol::Modulo => String::from("%"),
        Symbol::Power => String::from("^"),
        Symbol::Less => String::from("<"),
        Symbol::Greater => String::from(">"),
        Symbol::Dot => String::from("."),
        Symbol::Comma => String::from(", "),
        Symbol::Colon => String::from(":"),
        Symbol::Semicolon => String::from(";"),
        Symbol::RoundOpen => String::from("("),
        Symbol::RoundClose => String::from(")"),
        Symbol::SquareOpen => String::from("["),
        Symbol::SquareClose => String::from("]"),
        Symbol::CurlyOpen => String::from("{"),
        Symbol::CurlyClose => String::from("}"),
    }
}

fn sym_to_str(sym: &Symbol) -> Option<String> {
    match sym {
        Symbol::Str  => Some(String::from("Str")),
        Symbol::Num  => Some(String::from("Num")),
        Symbol::Bool => Some(String::from("Bool")),
        Symbol::Any  => Some(String::from("Any")),

        Symbol::Array(t) => {
            if **t == Symbol::Any { return Some(String::from("Table")) }

            let mut name = String::from("Array<");

            if let Some(inner_type) = sym_to_str(t) {
                name.push_str(&inner_type);
                name.push('>');
                
                Some(name)
            }
            else {
                None
            }
        }

        _ => None
    }
}

fn eval_exp(exp: Vec<Token>) -> String {
    let mut eval = String::new();

    for t in exp {
        match &t.symbol {
            Symbol::Custom(s) => eval.push_str(s),

            Symbol::Plus => eval.push('+'),
            Symbol::Minus => eval.push('-'),
            Symbol::Multiply => eval.push('*'),
            Symbol::Divide => eval.push('/'),
            Symbol::Modulo => eval.push('%'),

            Symbol::Equal => eval.push('='),
            Symbol::Less => eval.push('<'),
            Symbol::Greater => eval.push('>'),

            Symbol::Not => eval.push('~'),
            Symbol::Hash => eval.push('#'),
            Symbol::In => eval.push('§'),
            Symbol::Colon => eval.push('_'),

            Symbol::And => eval.push_str("and"),
            Symbol::Or => eval.push_str("or"),

            Symbol::Comma => eval.push(','),
            Symbol::Dot => eval.push('.'),

            Symbol::RoundOpen => eval.push('('),
            Symbol::RoundClose => eval.push(')'),

            Symbol::SquareOpen => eval.push('['),
            Symbol::SquareClose => eval.push(']'),

            _ => { println!("Can't eval: {:?}", t.symbol) }
        }
    }

    return eval;
}

fn eval_str(exp: String, e_type: &Symbol, vars: &HashMap<usize,HashMap<String, (Symbol, bool)>>, scope: usize, namespace: &String, using: &Vec<String>, token: &Token) -> String {
    let mut eval = exp;

    /*if exp.contains("..") { // TODO
        let part = exp.split("..").map(|s| s.to_owned()).collect::<Vec<String>>();
        eval.push('[');

        let mut i = part[0].parse::<i64>().unwrap();
        while i < part[1].parse::<i64>().unwrap() {
            eval.push_str(&format!("{},", i));

            i += 1;
        }
        eval.pop();
        eval.push(']');
    }
    else { eval = exp }*/
    //eval = exp;

    let exp_type = get_expr_type(&mut eval, &vars, scope, namespace, using, &token);
    if *e_type != Symbol::Any && exp_type != Symbol::Any && e_type != &exp_type && !(*e_type == Symbol::Array(Box::new(Symbol::Any)) && if let Symbol::Array(_) = exp_type {true} else {false} ) 
        { error(&format!("Expected `{}`, but got `{}`", sym_to_str(e_type).unwrap(), sym_to_str(&exp_type).unwrap()), &token) }

    if eval.starts_with("[") && eval.ends_with("]") { eval = eval.replace("[", "{").replace("]", "}") }

    return eval;
}

fn get_expr_type(
    exp: &mut String,
    vars: &HashMap<usize, HashMap<String, (Symbol, bool)>>,
    scope: usize,
    namespace: &String,
    using: &Vec<String>,
    token: &Token,
) -> Symbol {
    use Symbol::*;

    // 📦 Handle String Literals
    /*if (exp.starts_with('"') && exp.ends_with('"') {
        return Str;
    }*/

    // 🧮 Handle Arrays and Tables
    if exp.starts_with('[') && exp.ends_with(']') {
        let inner = &exp[1..exp.len() - 1].trim();

        if inner.is_empty() {
            return Any;
        }

        let mut elements = split_top_level_commas(inner);
        let mut types = vec![];

        for el in elements.iter_mut() {
            let el_type = get_expr_type(el, vars, scope, namespace, using, token);
            types.push(el_type);
        }

        if types.is_empty() {
            return Any;
        }

        let mut arr_type = &types[0];
        for t in &types[1..] {
            if t != arr_type || *arr_type == Symbol::Any {
                arr_type = &Symbol::Any;
                break;
            }
        }

        exp.clear();

        exp.push('{');
        for el in &elements {
            exp.push_str(el);
            exp.push(',');
        }
        if !elements.len() > 0 { exp.pop(); }
        exp.push('}');

        return Symbol::Array(Box::new(arr_type.clone()));
    }

    //println!("{:?}", exp);

    let mut copy = exp.clone();

    if let Some(paren_pos) = exp.find('(') {
        let mut fine = true;
        let mut fun = String::new();

        let mut b_scope = 0;
        let mut outside_scopes = 0;
        let mut string = false;

        let mut prev_c = ' ';

        for c in exp.chars() {
            fun.push(c);
            if prev_c != '\\' && c == '"' { string = !string } if string { continue }
            prev_c = c;
            if c == '(' { if b_scope == 0 { outside_scopes += 1 } b_scope += 1 } 
            if c == ')' { b_scope -= 1 }
            if b_scope == 0 && "+-*/%<>~=&|,.§".contains(c) { fine = false; copy = remove_function_args(exp); break }
            if outside_scopes > 1 { fine = false; break }
        }
        if fine && exp.ends_with(')') && paren_pos != 0 {
            let func_name = exp[..paren_pos].trim();
            let args_str = &exp[paren_pos + 1..exp.len() - 1];

            let mut uses: Option<String> = None;

            let mut func_type = None;
            for s in (0..=scope).rev() {
                if let Some(scope_vars) = vars.get(&s) {
                    if let Some((vtype, _)) = scope_vars.get(func_name) {
                        func_type = Some(vtype.clone());
                        break;
                    }
                }
            }
            if func_type.is_none() {
                for s in (0..=scope).rev() {
                    if let Some(scope_vars) = vars.get(&s) {
                        if let Some((vtype, _)) = scope_vars.get(&format!("{}{}", namespace, func_name)) {
                            func_type = Some(vtype.clone());
                            break;
                        }
                    }
                }
            }
            if func_type.is_none() {
                for name in using {
                    for s in (0..=scope).rev() {
                        if let Some(scope_vars) = vars.get(&s) {
                            if let Some((vtype, _)) = scope_vars.get(&format!("{}__{}", name, func_name)) {
                                uses = Some(name.to_owned());
                                func_type = Some(vtype.clone());
                                break;
                            }
                        }
                    }
                }
            }

            if func_type.is_none() {
                error(&format!("Undeclared function `{}`", func_name), token);
            }

            let mut args = split_top_level_commas(args_str);
            for arg in args.iter_mut() {
                get_expr_type(arg, vars, scope, namespace, using, token);
            }

            if let Some(ns) = uses {
                *exp = format!("{}__{}", ns, func_name);
            }
            else {
                *exp = func_name.to_owned();
            }

            exp.push('(');

            for arg in &args {
                exp.push_str(&arg);
                exp.push(',');
            }
            if args.len() > 0 { exp.pop(); }
            exp.push(')');

            return func_type.unwrap_or(Symbol::Any);
        }
    }

    if exp.contains('[') {
        return resolve_indexing_chain(exp, vars, scope, namespace, using, token);
    }

    // 💡 Resolve individual tokens in the expression
    let mut current_type = Any;
    let mut tokens = copy
        .split(|c: char| "+-*/%()<>~=&|,.§".contains(c))
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>();

    let mut str = false;

    //println!("\n{exp} -> {copy}");

    for token_str in &mut tokens {
        if token_str.is_empty() {
            continue;
        }

        let tok_type = 
        if token_str.starts_with('"') {
            str = true;
            Str
        } else if token_str.ends_with('"') && !token_str.ends_with("\\\"") {
            str = false;
            Str
        } else if str {
            Str
        } else if token_str.parse::<i64>().is_ok() || token_str.parse::<f64>().is_ok() || if let Some(s) = token_str.strip_prefix("0x") { u64::from_str_radix(s, 16).is_ok() } else { false } {
            Num
        } else if *token_str == "true" || *token_str == "false" {
            Bool
        } else if *token_str == "nil" {
            Any
        } else if token_str.contains('[') && token_str.ends_with(']') {
            let mut ts = token_str.to_string();
            let t = get_expr_type(&mut ts, vars, scope, namespace, using, token);
            *exp = exp.replace(*token_str, &ts);
            return t;
        } else {
            let mut found = false;
            let mut sym = Any;

            let mut len = false;

            if token_str.starts_with("#") { *token_str = token_str.strip_prefix("#").unwrap(); len = true }
   
            for s in (0..=scope).rev() {
                if let Some(scope_vars) = vars.get(&s) {
                    if let Some((vtype, _)) = scope_vars.get(&format!("{}{}", namespace, *token_str)) {
                        if !exp.contains(&format!("{}{}", namespace, *token_str)) { *exp = exp.replace(*token_str, &format!("{}{}", namespace, *token_str)) }
                        sym = vtype.clone();
                        found = true;
                        break;
                    }
                }
            }
            if !found {
                for s in (0..=scope).rev() {
                    if let Some(scope_vars) = vars.get(&s) {
                        //println!("\n\n\n{token_str:?} {s}: {scope_vars:?}");
                        if let Some((vtype, _)) = scope_vars.get(*token_str) {
                            sym = vtype.clone();
                            found = true;
                            break;
                        }
                    }
                }
            }
            if !found {
                for name in using {
                    for s in (0..=scope).rev() {
                        if let Some(scope_vars) = vars.get(&s) {
                            if let Some((vtype, _)) = scope_vars.get(&format!("{}__{}", name, *token_str)) {
                                if !exp.contains(&format!("{}__{}", name, *token_str)) { *exp = exp.replace(*token_str, &format!("{}__{}", name, *token_str)) }
                                sym = vtype.clone();
                                found = true;
                                break;
                            }
                        }
                    }
                }
            }

            //println!("{}", exp);

            if !found {
                error(&format!("Undeclared variable or function `{}`", *token_str), token);
            }

            if len && match sym { Symbol::Array(_) => false, _ => true } {
                error(&format!("Cannot take length of type `{}`", sym_to_str(&sym).unwrap()), token);
            }

            if len { Symbol::Num } else { sym }
        };

        //println!("{:?} | {:?} <- {}", current_type, tok_type, token_str);

        match (&current_type, &tok_type) {
            (_, Any) => {}
            (Any, _) => current_type = tok_type.clone(),
            (Num, Num) => {}
            (Bool, Bool) => {}
            (Str, Str) => {}
            (Str, Num) | (Str, Bool) | (Num, Str) | (Bool, Str) | (Num, Bool) | (Bool, Num) => {
                error(
                    &format!(
                        "Illegal operation between types `{}` and `{}`",
                        sym_to_str(&current_type).unwrap_or_default(),
                        sym_to_str(&tok_type).unwrap_or_default()
                    ),
                    token,
                );
            }
            _ => {}
        }
    }

    // 🔎 Detect boolean operators
    if exp.contains("==")
        || exp.contains("~=")
        || exp.contains(">=")
        || exp.contains("<=")
        || exp.contains("<")
        || exp.contains(">")
        || exp.contains("&&")
        || exp.contains("||")
    {
        *exp = exp.replace("&&", " and ");
        *exp = exp.replace("||", " or ");
        return Bool;
    } else if exp.contains('+')
        || exp.contains('-')
        || exp.contains('*')
        || exp.contains('/')
        || exp.contains('%')
    {
        return Num;
    } else if current_type != Any {
        return current_type;
    }

    Any
}

fn remove_function_args(input: &str) -> String {
    let mut output = String::new();
    let mut chars = input.chars().peekable();
    let mut stack = Vec::new();

    while let Some(c) = chars.next() {
        if c == '(' {
            // Look behind to check if it's a function call
            if let Some(prev) = output.chars().last() {
                if prev.is_alphanumeric() || prev == ')' {
                    output.push('(');
                    stack.push(')');
                    // Skip until matching ')'
                    let mut nested = 0;
                    while let Some(&next_c) = chars.peek() {
                        chars.next();
                        if next_c == '(' {
                            nested += 1;
                        } else if next_c == ')' {
                            if nested == 0 {
                                break;
                            }
                            nested -= 1;
                        }
                    }
                    output.push(')');
                    continue;
                }
            }
            output.push(c);
        } else {
            output.push(c);
        }
    }

    output
}

fn resolve_indexing_chain(
    expr: &mut String,
    vars: &HashMap<usize, HashMap<String, (Symbol, bool)>>,
    scope: usize,
    namespace: &String,
    using: &Vec<String>,
    token: &Token,
) -> Symbol {
    use Symbol::*;

    let full_expr = expr.clone();
    let mut current_type = Any;

    let mut processed_expr = String::new();
    let mut remaining_expr = full_expr.as_str();

    while let Some((container_range, index_range, rest_range)) = split_index_once(remaining_expr) {
        let mut container = remaining_expr[container_range.clone()].trim().to_string();
        let mut index = remaining_expr[index_range.clone()].trim().to_string();
        let rest = &remaining_expr[rest_range.clone()];

        // Type resolution for the container
        let container_type = if current_type == Any {
            let mut found = false;

            for s in (0..=scope).rev() {
                if let Some(scope_vars) = vars.get(&s) {
                    if let Some((vtype, _)) = scope_vars.get(&format!("{}{}", namespace, &container)) {
                        container = container.replace(&container, &format!("{}{}", namespace, &container));
                        current_type = vtype.clone();
                        found = true;
                        break;
                    }
                }
            }
            if !found {
                for s in (0..=scope).rev() {
                    if let Some(scope_vars) = vars.get(&s) {
                        if let Some((vtype, _)) = scope_vars.get(&container) {
                            current_type = vtype.clone();
                            found = true;
                            break;
                        }
                    }
                }
            }
            if !found {
                for name in using {
                    for s in (0..=scope).rev() {
                        if let Some(scope_vars) = vars.get(&s) {
                            if let Some((vtype, _)) = scope_vars.get(&format!("{}__{}", name, &container)) {
                                container = container.replace(&container, &format!("{}__{}", name, &container));
                                current_type = vtype.clone();
                                found = true;
                                break;
                            }
                        }
                    }
                }
            }

            if !found {
                error(&format!("Undeclared variable `{}`", container), token);
            }
            current_type.clone()
        } else {
            current_type.clone()
        };

        // Resolve the type of index (just for validation)
        let index_ty = get_expr_type(&mut index, vars, scope, namespace, using, token);

        //println!("{:?}", expr);

        match container_type {
            Array(inner) => {
                if *inner != Any && index_ty != Num && index_ty != Any {
                    error(
                        &format!(
                            "Invalid index type: expected `Num`, got `{}`",
                            sym_to_str(&index_ty).unwrap_or_default()
                        ),
                        token,
                    );
                }

                // Add +1 unless it’s already handled
                if !index.contains("+001") {
                    index = format!("{}+001", index);
                }

                current_type = *inner;
            }
            Any => return Any,
            other => {
                error(
                    &format!(
                        "Cannot index into non-array type `{}`",
                        sym_to_str(&other).unwrap_or_default()
                    ),
                    token,
                );
                return Any;
            }
        }

        // Build up the new expression
        if processed_expr.is_empty() {
            processed_expr = format!("{}[{}]", container, index);
        } else {
            processed_expr = format!("{}[{}]", processed_expr, index);
        }

        // Slice into the remaining part of the expr
        remaining_expr = rest;
    }

    // If no indexing was detected at all, return original type
    if processed_expr.is_empty() {
        return current_type;
    }

    *expr = processed_expr;
    current_type
}

fn split_index_once(expr: &str) -> Option<(Range<usize>, Range<usize>, Range<usize>)> {
    let mut depth = 0;
    let mut start = None;
    let mut end = None;

    for (i, c) in expr.char_indices() {
        if c == '[' {
            if depth == 0 {
                start = Some(i);
            }
            depth += 1;
        } else if c == ']' {
            depth -= 1;
            if depth == 0 {
                end = Some(i);
                break;
            }
        }
    }

    if let (Some(s), Some(e)) = (start, end) {
        let container = 0..s;
        let index_expr = s + 1..e;
        let rest = e + 1..expr.len();
        return Some((container, index_expr, rest));
    }

    None
}

fn split_top_level_commas(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut depth = 0;
    let mut in_string = false;
    let mut start = 0;
    let mut chars = s.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        match c {
            '"' => {
                in_string = !in_string;
                // Handle escaped quote
                if i > 0 && &s[i - 1..i] == "\\" {
                    in_string = !in_string; // flip back
                }
            }
            '[' | '(' if !in_string => depth += 1,
            ']' | ')' if !in_string => depth -= 1,
            ',' if !in_string && depth == 0 => {
                result.push(s[start..i].trim().to_string());
                start = i + 1;
            }
            _ => {}
        }
    }

    if start < s.len() {
        result.push(s[start..].trim().to_string());
    }

    result
}


fn preprocess(code: String) -> String {
    let mut fin = String::new();
    let mut token = String::new();

    let mut lua = false;

    let mut b = 0;
    let mut set = false;

    let mut comment = false;

    let mut prelast = ' ';
    let mut last = ' ';

    for c in code.chars().collect::<Vec<char>>() {
        if c == '\n' { comment = false }
        if comment { continue }
        fin.push(c); if !c.is_whitespace() { token.push(c) }
        if token.ends_with("lua") { lua = true }

        if lua { if c == '{' { b += 1 } else if c == '}' { b -= 1 } }
        if b == 1 && lua { set = true }
        if set && b == 0 { lua = false; set = false }

        if prelast == '-' && last == '-' && c != '[' && lua { comment = true; fin.pop(); fin.pop(); fin.pop(); }

        prelast = last;
        last = c;

        if set && lua && c.is_whitespace() && !token.ends_with("]") { token.push(']'); fin.push_str("--[[]]") }
    }

    return fin;
}

fn tokenize(filename: String) -> Option<Vec<Token>> {
    let mut tokens = Vec::new();

    let file = fs::read_to_string(filename.clone());
    if file.is_err() { return None }
    let mut file = file.unwrap().replace("\r", "");
    file = preprocess(file);

    //println!("\n\n{}\n\n", file);

    let code_chars = file.chars().collect::<Vec<char>>();
    let mut code = multipeek(code_chars.iter());

    let mut token = String::new();

    let mut prev_c = ' ';

    let mut line_number = 1;
    while code.len() > 0 {
        let mut next_c = *code.next().unwrap();
        while next_c.is_whitespace() { /*tokens.push(Token::new(Symbol::Custom(String::from(" ")), filename.clone(), line_number));*/ if next_c == '\n' { line_number += 1 } next_c = *code.next().unwrap_or(&&'\u{0}') }
        token.push(next_c);

        if token == "\"" {
            let start_line = line_number;
            next_c = *code.next().unwrap_or(&&'\u{0}'); 
            token.push(next_c);
            while next_c != '"' || prev_c == '\\' { 
                if next_c == '\n' { line_number += 1 } 
                prev_c = next_c;
                next_c = *code.next().expect(&format!("Unclosed string on line: {}", start_line)); 
                token.push(next_c);
            }
        }

        if !token.is_empty() && **code.peek_nth(0).unwrap() == ' ' && **code.peek_nth(1).unwrap() == 'i' && **code.peek_nth(2).unwrap() == 'n' {
            tokens.push(Token::new(Symbol::Custom(token.clone()), filename.clone(), line_number)); 
            token.clear(); 
            continue;
        }

        let mut peek_c = **code.peek().unwrap_or(&&'\u{0}');
        let peek_wc = peek_c;
        while peek_c.is_whitespace() { /*tokens.push(Token::new(Symbol::Custom(String::from(" ")), filename.clone(), line_number));*/ if next_c == '\n' { line_number += 1 } next_c = *code.next().unwrap(); peek_c = **code.peek().unwrap_or(&&'\u{0}') }
        if next_c == '\n' { line_number += 1 }

        if token == "/" && peek_c == '/' { while *code.next().unwrap_or(&&'\n') != '\n' {} line_number += 1; token.clear() }
        else if token == "/" && peek_c == '*' {
            let start_line = line_number;
            next_c = *code.next().unwrap_or(&&'\u{0}'); 
            while next_c != '*' || peek_c != '/' { 
                if next_c == '\n' { line_number += 1 } 
                next_c = *code.next().expect(&format!("Unclosen multiline comment on line: {}", start_line)); 
                peek_c = **code.peek().unwrap_or(&&'\u{0}') 
            } 
            code.next(); 
            token.clear();
        }

        //println!("{:?}", token);

        //println!("{}->{}\t{:?}", next_c, peek_wc, &token);

        if let Some(sym) = to_symbol(&token, &tokens.last()) {
            if token.len() == 1 || !peek_wc.is_alphabetic() || peek_wc.is_whitespace() {
                if sym == Symbol::Include && tokens.last().unwrap_or( &Token::sym(Symbol::Do)) == &Token::sym(Symbol::In) { tokens.pop(); }
                tokens.push(Token::new(sym, filename.clone(), line_number)); 
                token.clear(); 
            }
        }
        else if to_symbol(&peek_c.to_string(), &tokens.last()).is_some() && !token.starts_with("Array") { 
            tokens.push(Token::new(Symbol::Custom(token.clone()), filename.clone(), line_number)); 
            token.clear(); 
        }

        prev_c = next_c;
    }

    /*let mut lua = false;

    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i].symbol {
            Symbol::Custom(s) => { if !lua && s == " " { tokens.remove(i); i -= 1 } }
            Symbol::Lua => {lua = true; while &tokens[i].symbol != &Symbol::CurlyOpen { i += 1 } },
            Symbol::CurlyClose => lua = false,

            _ => {}
        }
        i += 1
    }*/

    return Some(tokens);
}

fn to_symbol(mut s: &str, last: &Option<&Token>) -> Option<Symbol> {

    let last = last.unwrap_or(&Token::new(Symbol::Not, String::new(), 0)).symbol.clone();

    let is_type = last == Symbol::Colon || last == Symbol::Greater;

    match s {
        //"and" => Some(Symbol::And),
        //"local" => Some(Symbol::Custom(String::from("local"))),
        //"then" => Some(Symbol::Custom(String::from("then"))),

        //"tion" => Some(Symbol::Custom(String::from("tion"))),
        //"not" => Some(Symbol::Custom(String::from("not"))),
        //"until" => Some(Symbol::Custom(String::from("until"))),

        //"or" => Some(Symbol::Or),

        //"elseif" => Some(Symbol::Custom(String::from("elseif"))),
        //"repeat" => Some(Symbol::Custom(String::from("repeat"))),

        //"end" => Some(Symbol::Custom(String::from("end"))),

        "lua" => Some(Symbol::Lua),

        "include" => Some(Symbol::Include),
        "clude" => Some(Symbol::Include),
        "namespace" => Some(Symbol::Namespace),
        "using" => Some(Symbol::Using),
        "func" => Some(Symbol::Func),
        "return" => Some(Symbol::Return),

        "loop" => Some(Symbol::Loop),
        "while" => Some(Symbol::While),
        "do" => Some(Symbol::Do),
        "for" => Some(Symbol::For),
        "in" => Some(Symbol::In),
        "break" => Some(Symbol::Break),
        "goto" => Some(Symbol::Goto),
        "continue" => Some(Symbol::Continue),

        "if" => Some(Symbol::If),
        "else" => Some(Symbol::Else),

        "let" => Some(Symbol::Let),
        "mut" => Some(Symbol::Mut),

        "||" => Some(Symbol::Or),
        "&&" => Some(Symbol::And),

        "!" => Some(Symbol::Not),
        "#" => Some(Symbol::Hash),
        "§" => Some(Symbol::In),

        "=" => Some(Symbol::Equal),
        "+" => Some(Symbol::Plus),
        "-" => Some(Symbol::Minus),
        "*" => Some(Symbol::Multiply),
        "/" => Some(Symbol::Divide),
        "%" => Some(Symbol::Modulo),
        "^" => Some(Symbol::Power),

        "<" => Some(Symbol::Less),
        ">" => Some(Symbol::Greater),

        "." => Some(Symbol::Dot),
        "," => Some(Symbol::Comma),
        ":" => Some(Symbol::Colon),
        ";" => Some(Symbol::Semicolon),

        "(" => Some(Symbol::RoundOpen),
        ")" => Some(Symbol::RoundClose),
        "[" => Some(Symbol::SquareOpen),
        "]" => Some(Symbol::SquareClose),
        "{" => Some(Symbol::CurlyOpen),
        "}" => Some(Symbol::CurlyClose),

        /*"0" => Some(Symbol::Custom(String::from("0"))),
        "1" => Some(Symbol::Custom(String::from("1"))),
        "2" => Some(Symbol::Custom(String::from("2"))),
        "3" => Some(Symbol::Custom(String::from("3"))),
        "4" => Some(Symbol::Custom(String::from("4"))),
        "5" => Some(Symbol::Custom(String::from("5"))),
        "6" => Some(Symbol::Custom(String::from("6"))),
        "7" => Some(Symbol::Custom(String::from("7"))),
        "8" => Some(Symbol::Custom(String::from("8"))),
        "9" => Some(Symbol::Custom(String::from("9"))),*/

        _ => {
            if is_type {
                match s {
                    "Str" => return Some(Symbol::Str),
                    "Num" => return Some(Symbol::Num),
                    "Bool" => return Some(Symbol::Bool),
                    "Any" => return Some(Symbol::Any),

                    "Table" => return Some(Symbol::Array(Box::new(Symbol::Any))),
                    _ => {}
                }
                if s.starts_with("Array<") {
                    s = s.strip_prefix("Array").unwrap();

                    let chars = s.chars().collect::<Vec<char>>();

                    let mut quacks = 0;

                    for c in chars {
                        if c == '<' { quacks += 1 }
                        else if c == '>' { quacks -= 1 }
                    }

                    if quacks != 0 { return None }

                    s = s.strip_prefix("<").unwrap().strip_suffix(">").unwrap();
                    
                    return Some(Symbol::Array(Box::new(to_symbol(s, &Some(&Token::new(Symbol::Colon, String::new(), 0))).unwrap())))
                }
            }
            return None;
        }
    }
}
