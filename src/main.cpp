#include <cstdlib>
#include <iostream>
#include <vector>
#include <cctype>
#include <sstream>
#include <deque>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/EHPersonalities.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/InitializePasses.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/CodeGen/CommandFlags.inc"

using namespace llvm;

typedef enum TokenType {
    TOKEN_INT,
    TOKEN_PLUS,
    TOKEN_HYPHEN,
    TOKEN_ASTERISK,
    TOKEN_SLASH,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_EOF,
} TokenType;

class Token {
private:
    std::string token_str;
    TokenType type;
public:
    Token(std::string token_str, TokenType type) : token_str(token_str), type(type) {};
    std::string as_str();
    TokenType get_token_type();
};

std::string Token::as_str() {
    return token_str;
}

TokenType Token::get_token_type() {
    return type;
}

class Lexer {
private:
    std::string source;
    int cur_pos;
    std::deque<Token> peeked;

    char cur_char();
    char read_char();
    Token tokenize_number();
    Token tokenize_symbol();
    Token next();
public:
    Lexer(std::string source) : source(source) {};
    std::string type_to_str(TokenType type);
    Token peek(unsigned int offset);
    Token read();
    Token curr();
};

char Lexer::read_char() {
    return source[cur_pos++];
}

char Lexer::cur_char() {
    return source[cur_pos];
}

Token Lexer::tokenize_number() {
    int begin = cur_pos;
    while (std::isdigit(cur_char())) {
        read_char();
    }
    int end = cur_pos;
    
    std::string result_str = source.substr(begin, end - begin);
    return Token(result_str, TOKEN_INT);
}

Token Lexer::tokenize_symbol() {
    char op = read_char();
    if (op == '+') {
        return Token("+", TOKEN_PLUS);
    } else if (op == '-') {
        return Token("-", TOKEN_HYPHEN);
    } else if (op == '*') {
        return Token("*", TOKEN_ASTERISK);
    } else if (op == '/') {
        return Token("/", TOKEN_SLASH);
    } else if (op == '(') {
        return Token("(", TOKEN_LPAREN);
    } else if (op == ')') {
        return Token(")", TOKEN_RPAREN);
    } else {
        std::stringstream stream;
        stream << "Error: unknown operator: " << op << std::endl;
        throw stream.str();
    }
}

Token Lexer::next() {
    if (cur_pos >= source.size()) {
        return Token("\0", TOKEN_EOF);
    }
    if (std::isspace(cur_char())) {
        while (std::isspace(cur_char())) read_char();
    }
    if (std::isdigit(cur_char())) {
        return tokenize_number();
    } else if (std::ispunct(cur_char())) {
        return tokenize_symbol();
    } else {
        std::stringstream stream;
        stream << "Error: unknown character: " << cur_char() << std::endl;
        throw stream.str();
    }
}

std::string Lexer::type_to_str(TokenType type) {
    switch (type) {
    case TOKEN_INT:
        return "Int";
    case TOKEN_PLUS:
        return "+";
    case TOKEN_HYPHEN:
        return "-";
    case TOKEN_ASTERISK:
        return "*";
    case TOKEN_SLASH:
        return "/";
    default:
        return "`Unknown`";
    }
}

Token Lexer::peek(unsigned int offset) {
    if (offset < peeked.size())
        return peeked[offset];
    
    for (int i = 0; i <= offset; i++) {
        peeked.push_back(next());
    }

    Token token = peeked.back();
    return token;
}

Token Lexer::read() {
    if (peeked.size() != 0) {
        Token token = peeked.front();
        peeked.pop_front();
        return token;
    }

    return next();
}

Token Lexer::curr() {
    return peek(0);
}

class Compiler {
public:
    LLVMContext context;
    IRBuilder<> builder;
    std::unique_ptr<Module> module;
    
    Compiler() : builder(context), module{std::make_unique<Module>("code", context)} {}
    void add_function(std::string name);
    void generate_obj_file(std::string name);
};

void Compiler::add_function(std::string name) {
    std::vector<Type *> arg_types;
    Type *ret_type = Type::getInt32Ty(context);
    FunctionType *ft = FunctionType::get(ret_type, arg_types, false);
    Function *f = Function::Create(ft, Function::ExternalLinkage, name, module.get());

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "entry", f);
    builder.SetInsertPoint(bb);
    llvm::verifyFunction(*f);
}

void Compiler::generate_obj_file(std::string name) {
    auto target_triple = llvm::sys::getDefaultTargetTriple();
    std::string error_str;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, error_str);
    if (!target) {
        llvm::errs() << error_str;
        exit(1);
    }

    auto cpu = "generic";
    auto features = "";

    auto rm = llvm::Optional<llvm::Reloc::Model>();
    TargetOptions opt;//  = InitTargetOptionsFromCodeGenFlags();
    TargetMachine *target_machine =
        target->createTargetMachine(target_triple, cpu, features, opt, rm);
    module->setDataLayout(target_machine->createDataLayout());

    std::error_code ec;
    llvm::raw_fd_ostream dest(name, ec, llvm::sys::fs::OF_None);

    if (ec) {
        llvm::errs() << "Could not open file: " << ec.message();
        exit(1);
    }

    TargetLibraryInfoImpl TLII(Triple(module->getTargetTriple()));
    llvm::legacy::PassManager pass;

    auto file_type = llvm::CGFT_ObjectFile;
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type,
                                            true)) {
        llvm::errs() << "target_machine can't emit a file of this type";
        exit(1);
    }

    pass.run(*module);
    dest.flush();
}

class Expr {
public:
    virtual ~Expr() = default;

    virtual Value *codegen(Compiler &c) = 0;
    virtual std::string to_str() = 0;
};

class IntegerExpr : public Expr {
    int number;
public:
    IntegerExpr(int number) : number(number) {};
    Value *codegen(Compiler &c) override;
    std::string to_str() override;
};

Value *IntegerExpr::codegen(Compiler &c) {
    return ConstantInt::get(c.context, APInt(32, number, true));
}

std::string IntegerExpr::to_str() {
    std::stringstream stream;
    stream << number;
    return stream.str();
}

class BinaryExpr : public Expr {
private:
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
    std::string op;
public:
    BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, std::string op)
        : lhs{std::move(lhs)}, rhs{std::move(rhs)}, op(op) {};
    Value *codegen(Compiler &c) override;
    std::string to_str() override;
};

llvm::Value *BinaryExpr::codegen(Compiler &c) {
    Value *lval = lhs->codegen(c);
    Value *rval = rhs->codegen(c);
    if (op == "+") {
        return c.builder.CreateAdd(lval, rval, "addtmp");
    } else if (op == "-") {
        return c.builder.CreateSub(lval, rval, "subtmp");
    } else if (op == "*") {
        return c.builder.CreateMul(lval, rval, "multmp");
    } else if (op == "/") {
        return c.builder.CreateUDiv(lval, rval, "divtmp");
    } else {
        std::stringstream stream;
        stream << "Error: unknown operator: " << op << std::endl;
        throw stream.str();
    }
}

std::string BinaryExpr::to_str() {
    std::stringstream stream;
    stream << "(" << lhs->to_str() << op << rhs->to_str() << ")";
    return stream.str();
}

class Parser {
private:
    Lexer l;

    bool match(TokenType type);
    Token expect(TokenType type);
public:
    Parser(Lexer l) : l(l) {}
    std::unique_ptr<Expr> primary_expr();
    std::unique_ptr<Expr> mul_expr();
    std::unique_ptr<Expr> add_expr();
};

bool Parser::match(TokenType type) {
    return l.curr().get_token_type() == type;
}

Token Parser::expect(TokenType type) {
    if (l.curr().get_token_type() != type) {
        std::stringstream stream;
        stream << "Error: expected " << l.type_to_str(type) << " but got " << l.type_to_str(l.curr().get_token_type()) << std::endl;
        throw stream.str();
    }

    return l.read();
}

std::unique_ptr<Expr> Parser::primary_expr() {
    if (match(TOKEN_INT)) {
        return std::make_unique<IntegerExpr>(IntegerExpr(std::stoi(l.read().as_str())));
    } else if (match(TOKEN_LPAREN)) {
        l.read();
        std::unique_ptr<Expr> expr = add_expr();
        expect(TOKEN_RPAREN);
        return expr;
    } else {
        std::stringstream stream;
        stream << "Error: can't parse primary expression" << std::endl;
        throw stream.str();
    }
}

std::unique_ptr<Expr> Parser::mul_expr() {
    auto lhs = primary_expr();
    for (; match(TOKEN_ASTERISK) || match(TOKEN_SLASH); ) {
        Token op = l.read();
        auto rhs = primary_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs), op.as_str());
    }

    return lhs;
}

std::unique_ptr<Expr> Parser::add_expr() {
    auto lhs = mul_expr();
    for (; match(TOKEN_PLUS) || match(TOKEN_HYPHEN); ) {
        Token op = l.read();
        auto rhs = mul_expr();
        lhs = std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs), op.as_str());
    }

    return lhs;
}

int main() {
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    try {
        Lexer l("2 + 3 * 4");
        Parser p(l);
        Compiler c;
        c.add_function("main");
        auto expr = p.add_expr();
        Value *val = expr->codegen(c);
        c.builder.CreateRet(val);
        c.module->print(errs(), nullptr);
        c.generate_obj_file("main.o");
    } catch(std::string msg) {
        std::cout << msg;
        std::exit(1);
    }

    return 0;
}
