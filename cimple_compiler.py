import sys
import string

from abc import ABC, abstractmethod

inputFileName = sys.argv[1] # second command line argument (first is the program name)
input_program = "" # the string which will save the symbols of the input program code

file = open(inputFileName, 'r') 
  
file_contents = file.read()
contents_split = file_contents.splitlines(True)

for line in contents_split:
    chars = [char for char in line]

    for char in chars:
        input_program += char

file.close() 

# here I assign a tokens for every valid unit

programTk, declareTk, functionTk, procedureTk, inTk, inoutTk = 0, 1, 2, 3, 4, 5
ifTk, elseTk, whileTk, switchcaseTk, caseTk, defaultTk, forcaseTk, incaseTk = 6, 7, 8, 9, 10, 11, 12, 13
returnTk, callTk, printTk, inputTk, notTk, andTk, orTk = 14, 15, 16, 17, 18, 19, 20
equalToTk, lesserThanTk, greaterThanTk, greaterOrLesserThanTk, lesserOrEqualToTk, greaterOrEqualToTk = 21, 22, 23, 24, 25, 26
plusTk, minusTk, multiplyTk, divideTk = 27, 28, 29, 30
integerValueTk, IDTk, assignTk, dotTk = 31, 32, 33, 34
semicolonTk, colonTk, commaTk, leftBracketTk, rightBracketTk, leftBraceTk, rightBraceTk, leftParenthesisTk, rightParenthesisTk = 35, 36, 37, 38, 39, 40, 41, 42, 43

# unit token assignment ends, now we just need to assign each unit string to its token (with the dictionary below)

tokens = {"program": programTk, "declare": declareTk, "function": functionTk, "procedure": procedureTk,
          "in": inTk, "inout": inoutTk, "if": ifTk, "else": elseTk, "while": whileTk, "switchcase": switchcaseTk,
          "case": caseTk, "default": defaultTk, "forcase": forcaseTk, "incase": incaseTk, "return": returnTk,
          "call": callTk, "print": printTk, "input": inputTk, "not": notTk, "and": andTk, "or": orTk,
          "=": equalToTk, "<": lesserThanTk, ">": greaterThanTk, "<>": greaterOrLesserThanTk, "<=": lesserOrEqualToTk,
          ">=": greaterOrEqualToTk, "+": plusTk, "-": minusTk, "*": multiplyTk, "/": divideTk, ":=": assignTk,
          ".": dotTk, ";": semicolonTk, ":": colonTk, ",": commaTk, "[": leftBracketTk, "]": rightBracketTk,
          "{": leftBraceTk, "}": rightBraceTk, "(": leftParenthesisTk, ")": rightParenthesisTk}

letters = string.ascii_letters # will be used for checking if a unit is a letter
digits = string.digits # will be used for checking if a unit is a digit
symbols = [char for char in input_program] # splitting program into characters
symbol_counter = 0 # counter that indexes the symbols
symbol_counter_limit = len(input_program) # used for checking about EOF
line = 1 # updated each time we come across '\n'

# Intermediate code subroutines and variables
#____________________________________________

quad_label = -1 # quad label, increases by 1 for each new quad (first one's 0)
quad_list = [] # the list of our quads, including their labels
temp_counter = -1 # each time we create a new temporary variable, it's index increases by one (T_0, T_1, T_2, T_3, ...)
let_C_code_be_created = True # set to false if we encounter function/procedure declarations/calls
C_variable_names = [] # when we declare variables in the Cimple code, we need to keep the for the C code as well

def nextquad(): # next quad label
    global quad_label
    return (quad_label + 1)

def genquad(op, x, y, z): # create new quad and add it to the quad list
    global quad_label
    quad_label += 1
    quad = str(quad_label) + ": " + str(op) + " , " + str(x) + " , " + str(y) + " , " + str(z) 
    # print(quad)
    quad_list.append(quad)

def newtemp(): # new temporal variable
    global temp_counter 
    temp_counter += 1
    return "T_" + str(temp_counter)

def emptylist():
    return []

def makelist(x):
    return [x]

def merge(list1, list2):
    return list1 + list2

def backpatch(index_list, z):
    for index in index_list:
        splitted_quad = quad_list[index].split()
        splitted_quad[-1] = str(z)
        quad_list[index] = ' '.join(splitted_quad)


#_______________________________________________



# Symbol matrix classes, variables and functions
#_______________________________________________

scopeCounter = 0
recordScopes = [] # scopes stored here will be deleted once their time comes 
finalCodeLevels = {}

class Entity(ABC):
    def __init__(self, entityName):
        self.entityName = entityName
    
    @abstractmethod
    def printEntity(self):
        pass

    def getEntityName(self):
        return self.entityName

class VarEntity(Entity):
    def __init__(self, entityName, offset):
        Entity.__init__(self, entityName)
        self.offset = offset
        self.args = []

    def setOffset(self, offset):
        self.offset = offset

    def getOffset(self):
        return self.offset

    def printEntity(self):
        return "<-- " + self.entityName + " / " + str(self.offset)

class funcEntity(Entity):
    def __init__(self, entityName):
        Entity.__init__(self, entityName)
        self.startQuad = -1
        self.args = []
        self.framelength = -1
        self.finalCodeLevel = "L_default"

    def setStartQuad(self, quad_label):
        self.startQuad = quad_label   

    def getStartQuad(self):
        return self.startQuad

    def addArgument(self, arg):
        self.args.append(arg) 

    def setFinalCodeLevel(self, level):
        self.finalCodeLevel = level

    def getFinalCodeLevel(self):
        return self.finalCodeLevel 

    def getArgumentParmodes(self):
        parmodes = [arg.getParmode() for arg in self.args]
        return parmodes

    def setFramelength(self, framelength):
        self.framelength = framelength

    def getFramelength(self):
        return self.framelength

    def printEntity(self):
        pars = ""
        for arg in self.args:
            pars+= " - " + arg.getParmode()
        return "<-- " + self.entityName + " / " + str(self.startQuad) + " / " + str(self.framelength) + " / (" + pars + ")"

class parEntity(Entity):
    def __init__(self, entityName, parMode, offset):
        Entity.__init__(self, entityName)
        self.parMode = parMode
        self.offset = offset

    def setOffset(self, offset):
        self.offset = offset

    def getOffset(self):
        return self.offset

    def getParMode(self):
        return self.parMode

    def printEntity(self):
        return "<-- " + self.entityName + " / " + str(self.offset) + " / " + self.parMode

class RecordScope:
    def __init__(self, nestingLevel):
        self.nestingLevel = nestingLevel
        self.entities = []
        self.nextRecordScope = None 
        self.offset = 8

    def addEntity(self, entity, isFunc = False):
        if not isFunc == True:  
            self.offset += 4
            entity.setOffset(self.offset)
        self.entities.append(entity)

    def getEntities(self):
        return self.entities

    def funcEntityAddArgument(self, argument):
        self.entities[-1].addArgument(argument)

    def setNextRecordScope(self, recordScope):
        self.nextRecordScope = recordScope

    def printRecordScope(self):
        retStr = str(self.nestingLevel) + " "
        for entity in self.entities:
            retStr += entity.printEntity() + " "
        return retStr

class Argument:
    def __init__(self, parmode):
        self.parmode = parmode
        self.nextArg = None
    
    def setNextArg(self, arg):
        self.nextArg = arg

    def getParmode(self):
        return self.parmode
    
def checkForRedeclaration(name, scopeLevel, line):
    for entity in recordScopes[scopeLevel].getEntities():
        if entity.getEntityName() == name and not isinstance(entity, parEntity):
            print("Semantic analysis error in line", line ,": '", name, "' redeclared in the same scope.")
            print("Note: functions, procedures and variables in the same scope must all have different names.")
            exit()

def searchForDeclaration(name, type, line):
    
    if type == "var":
        for i in range(scopeCounter - 1, -1, -1):
            for entity in recordScopes[i].getEntities():
                if entity.getEntityName() == name and (isinstance(entity, VarEntity) or isinstance(entity, parEntity)): # check if it's a variable entity (needs to be of the same type)
                    return entity, i
        print("Semantic analysis error in line", line , ": variable '", name, "' used but not declared in the same or one of the outer scopes.")
        exit()
    elif type == "func":
        for i in range(scopeCounter - 1, -1, -1):
            for entity in recordScopes[i].getEntities():
                if entity.getEntityName() == name and isinstance(entity, funcEntity): # check if it's a function entity (needs to be of the same type)
                    return entity, i
        print("Semantic analysis error in line", line , ": function '", name, "' used but not declared in the same or one of the outer scopes.")
        exit()

symbolFileName = inputFileName.split(".")[0] + ".symbolmatrix"
symbol_file = open(symbolFileName, 'w')
    
#_______________________________________________

# Final code objects and functions 
#_______________________________________________

final_code_lines = []
register_number = 1

def gnvlcode(name):
    levels_up = 0 # count how many levels up we are searching
    final_code_lines.append("   lw $t0,-4($sp)")
    for i in range(scopeCounter - 2, -1, -1):
        for entity in recordScopes[i].getEntities():
            if entity.getEntityName() == name and (isinstance(entity, VarEntity) or isinstance(entity, parEntity)): # check if it's a variable entity (needs to be of the same type)
                offset = entity.getOffset()
                final_code_lines.append("   addi $t0,$t0,-" + str(offset))
                return
        if levels_up > 1:
            final_code_lines.append("   lw $t0,-4($t0)")
        levels_up += 1

def findType(v): # helper for loadvr function
    currentScope = scopeCounter - 1
    if v in digits:
        return "constant", 0 
    for scope in range(currentScope, -1, -1):
        for entity in recordScopes[scope].getEntities():
            if entity.getEntityName() == v and not isinstance(entity, funcEntity):
                if scope == currentScope and (isinstance(entity, VarEntity) or (isinstance(entity, parEntity) and entity.getParMode() == "in")):
                    return "local", entity.getOffset() 
                elif scope == currentScope and isinstance(entity, parEntity) and entity.getParMode() == "inout":
                    return "parameter_by_reference", entity.getOffset()
                elif scope < currentScope and (isinstance(entity, VarEntity) or (isinstance(entity, parEntity) and entity.getParMode() == "in")) and scope > 0:
                    return "ancestor", entity.getOffset()
                elif scope < currentScope and isinstance(entity, parEntity) and entity.getParMode() == "inout" and scope > 0:
                    return "ancestor_by_reference", entity.getOffset()
                elif scope == 0:
                    return "global", entity.getOffset()
    return "error", -1

def loadvr(v, r):
    vType, offset = findType(v)
    if vType == "constant":
        final_code_lines.append("   li " + r + "," + str(v))
    elif vType == "global":
        final_code_lines.append("   lw " + r + ",-" + str(offset) + "($s0)")
    elif vType == "local":
        final_code_lines.append("   lw " + r + ",-" + str(offset) + "($sp)")
    elif vType == "parameter_by_reference":
        final_code_lines.append("   lw $t0,-" + str(offset) + "($sp)")
        final_code_lines.append("   lw " + r + ",($t0)")
    elif vType == "ancestor":
        gnvlcode(v)
        final_code_lines.append("   lw " + r + ",($t0)")
    elif vType == "ancestor_by_reference":
        gnvlcode(v)
        final_code_lines.append("   lw $t0,($t0)")
        final_code_lines.append("   lw " + r + ",($t0)")

def storerv(r, v):
    vType, offset = findType(v)
    if vType == "global":
        final_code_lines.append("   sw " + r + ",-" + str(offset) + "($s0)")
    elif vType == "local":
        final_code_lines.append("   sw " + r + ",-" + str(offset) + "($sp)")
    elif vType == "parameter_by_reference":
        final_code_lines.append("   lw $t0,-" + str(offset) + "($sp)")
        final_code_lines.append("   sw " + r + ",($t0)")
    elif vType == "ancestor":
        gnvlcode(v)
        final_code_lines.append("   sw " + r + ",($t0)")
    elif vType == "ancestor_by_reference":
        gnvlcode(v)
        final_code_lines.append("   lw $t0,($t0)")
        final_code_lines.append("   sw " + r + ",($t0)")


def findParDepth(par, functionScope):
    if par in digits:
        return "constant"
    for scope in range(functionScope, -1, -1):
        for entity in recordScopes[scope].getEntities():
            if entity.getEntityName() == par and not isinstance(entity, funcEntity):
                if scope == functionScope and (isinstance(entity, VarEntity) or (isinstance(entity, parEntity) and entity.getParMode() == "in")):
                    return "local"
                elif scope == functionScope and isinstance(entity, parEntity) and entity.getParMode() == "inout":
                    return "parameter_by_reference"
                elif scope < functionScope and (isinstance(entity, VarEntity) or (isinstance(entity, parEntity) and entity.getParMode() == "in")):
                    return "ancestor"
                elif scope < functionScope and isinstance(entity, parEntity) and entity.getParMode() == "inout":
                    return "ancestor_by_reference"
    print("Warning: findParDepth didn't work as expected!")
    return "error"
#_______________________________________________

def lex():
    global symbol_counter # we need to change the count globally
    global line # it is important we store the line info for printing the line in case of an error - easier to catch during lex()
    state = "start" # the initial state
    unit_symbols = [] # the symbols of the return unit, we will append the one by one as we read them
    commentStartLine = 0 # this is used if we detect the start of a comment and it is not closed before EOF. In this case we need to know the starting line of the comment as well

    while(state != "end" and state != "error"): # automato restarts if we arrive at these states
        if symbol_counter >= symbol_counter_limit : # we only arrive here if we don't recognise essential program units
            print("Lexical analysis error: program is incomplete.")
            exit()
            break
        if state == "start" :
            if symbols[symbol_counter] == " " or symbols[symbol_counter] == "\t": # we ignore empty spaces
                state = "start"
                symbol_counter += 1
                continue 
            if symbols[symbol_counter] == "\n": # we must count the lines
                line += 1
                state = "start"
                symbol_counter += 1
                continue 
            if symbols[symbol_counter] in letters:
                state = "ID"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            if symbols[symbol_counter] in digits:
                state = "arithmetic_unit"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            if symbols[symbol_counter] in ["+", "-", "*", "/"]: # nothing else to do other than recognise those 
                state = "end"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            if symbols[symbol_counter] == "#": 
                commentStartLine = line
                state = "comment"
                symbol_counter += 1
                continue
            if symbols[symbol_counter] == "<": # there are a lot of possibilities to check...
                if(symbols[symbol_counter + 1] == "=" or symbols[symbol_counter + 1] == ">"): #... so we do forward checking to see if we have a unit with two symbols
                    state = "end"
                    unit_symbols.append(symbols[symbol_counter])
                    unit_symbols.append(symbols[symbol_counter + 1])
                    symbol_counter += 2
                    continue
                else:
                    state = "end"
                    unit_symbols.append(symbols[symbol_counter])
                    symbol_counter += 1
                    continue
            if symbols[symbol_counter] == ">": # similar to "<" here
                if(symbols[symbol_counter + 1] == "="):
                    state = "end"
                    unit_symbols.append(symbols[symbol_counter])
                    unit_symbols.append(symbols[symbol_counter + 1])
                    symbol_counter += 2
                    continue
                else:
                    state = "end"
                    unit_symbols.append(symbols[symbol_counter])
                    symbol_counter += 1
                    continue
            if symbols[symbol_counter] == "=":
                state = "end"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            if symbols[symbol_counter] == ":": # only the ':=' unit is valid in this language 
                state = "colon"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            if symbols[symbol_counter] in ["[", "]", "(", ")" , "{" , "}"]:
                state = "end"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            if symbols[symbol_counter] in [";", ",", "."]:
                state = "end"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            print("Lexical analysis error in line", line, ": the symbol", symbols[symbol_counter], "doesn't belong in the language's grammar.")
            exit()

        if state == "ID" : # here we recognise keywords and ID's
            if symbols[symbol_counter] in letters:
                state = "ID"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            elif symbols[symbol_counter] in digits:
                state = "ID"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            else: # that means we have reached the end of the ID
                state = "potential_ID"
                continue

        if state == "potential_ID" : # before we accept, we need to make sure that the characters do not exceed the limit
            ID = ""
            for char in unit_symbols:
                ID += char
            if len(ID) > 30:
                print("Lexical analysis error: keyword/ID", ID, "in line", line, "exceeds the limit of 30 characters, which is not allowed.")
                exit()
            else:
                state = "end"
                continue

        if state == "arithmetic_unit" : 
            if symbols[symbol_counter] in digits:
                state = "arithmetic_unit"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            else: # that means we have reached the end of the arithmetic unit
                state = "potential_number"
                continue
        if state == "comment" :
            if symbol_counter == len(symbols) - 1:
                print("Lexical analysis error: comment that started at line", commentStartLine, "was not closed before reaching EOF.")
                exit()
            elif symbols[symbol_counter] == "\n": # we must count the lines
                line += 1
                state = "comment"
                symbol_counter += 1
                continue 
            elif symbols[symbol_counter] != "#": # comments end when another '#' is met
                state = "comment"
                symbol_counter += 1
                continue
            else:
                state = "start"
                symbol_counter += 1
                continue
            
        if state == "potential_number": # before we accept a number, we must make sure that it inside the language's limits
            if symbols[symbol_counter] in letters:
                unit = ""
                for char in unit_symbols:
                    unit += char
                print("Lexical analysis error in line", line, ": letters not allowed in number units (", unit, ").")
                exit()       
            else:
                unit = ""
                for char in unit_symbols:
                    unit += char
                number = int(unit)
                if number > (2**32 - 1) or number < -(2**32 - 1):
                    print("Lexical analysis error in line", line, ": the number", number, "exceeds the language's limits. Every integer must be in the range [-(2^32 - 1), (2^32 - 1)]")
                    exit()     
                state = "end"
                continue   

        if state == "colon" : 
            if symbols[symbol_counter] == "=":
                state = "end"
                unit_symbols.append(symbols[symbol_counter])
                symbol_counter += 1
                continue
            else: 
                print("Lexical analysis error in line", line, ": only '=' accepted after ':' symbol (assign operator ':=').")
                exit()
        
    unit = ""
    
    for symbol in unit_symbols: # concatenate the symbols
        unit += symbol

    tk = 0
    if unit in list(tokens.keys()): # we need to recongise keywords as such
        tk = tokens[unit]
    elif unit[0] in digits: # this means we have a number and need to recognise it as such
        tk = integerValueTk
    else: # everything else is a lexical unit that is not a keyword (variables etc)
        tk = IDTk
        
    # print(unit, line) 
    return(unit, tk, line)

startingUnit, startingTk, startingLine = lex() # we need to 'feed' the syntax analyser the starting token
# from now on, we just used the lex() function to return a lexical unit whenever we neeed it and just do the syntax analysis according to the grammar's rules

def syntaxAnalysisAndIntCodeCreation(): 
    # start syntax analysis 
    program()
    # syntax analysis ended successfully

    # now right the intermediate code quads to an .int file
    intFileName = inputFileName.split(".")[0] + ".int"
    with open(intFileName, 'w') as the_int_file:
        for i in range(len(quad_list)):
            the_int_file.write(quad_list[i])
            the_int_file.write("\n")
    # successfully written intermediate code 

    # now right the final code quads to an .asm file
    finalFileName = inputFileName.split(".")[0] + ".asm"
    with open(finalFileName, 'w') as the_final_file:
        for i in range(len(final_code_lines)):
            the_final_file.write(final_code_lines[i])
            the_final_file.write("\n")
    # successfully written final code 
    
def create_C_code():
    level_counter = -1
    levels_dict = {}

    for quad in quad_list:
        level_counter += 1
        level = "L_" + str(level_counter)
        line = ""
        quad = quad.split()
        quad = [elem for elem in quad if elem != ","]
        if quad[1] == ':=':
            line = line + quad[4] + " = " + quad[2]
        elif quad[1] in ['+', '*', '/', '-']:
            line = line + quad[4] + " = " + quad[2] + " " + quad[1] + " " + quad[3]
        elif quad[1] in ['<', '>', '=', '<=', '>=', '<>']:
            if quad[1] != "=" and quad[1] != "<>":
                line = line + "if(" + quad[2] + " " + quad[1] + " " + quad[3] + ") goto " + "L_" + quad[4]
            elif quad[1] == "=":
                line = line + "if(" + quad[2] + " == " + quad[3] + ") goto " + "L_" + quad[4]
            else:
                line = line + "if(" + quad[2] + " != " + quad[3] + ") goto " + "L_" + quad[4]
        elif quad[1] == 'jump':
            line += "goto L_" + quad[4] 
        elif quad[1] == 'retv':
            line += "return " + quad[2]
        elif quad[1] == 'out':
            line += 'printf("%' + 'd", ' + quad[2] + ')'  
        elif quad[1] == 'inp':
            line += 'scanf("%' + 'd", &' + quad[2] + ')' 
        line += "; //(" + quad[1] + ", " + quad[2] + ", " + quad[3] + ", " + quad[4] + ")"
        levels_dict[level] = line
    
    # time to write the lines to our C file
    C_FileName = inputFileName.split(".")[0] + ".c"
    with open(C_FileName, 'w') as the_C_file:
        the_C_file.write("int main()\n")
        the_C_file.write("{\n")
        if len(C_variable_names) > 0:
            var_list = ""
            for i in range(len(C_variable_names)):
                if i != len(C_variable_names) - 1:
                    var_list += C_variable_names[i] + ", "
                else:
                    var_list += C_variable_names[i] + ";"
            the_C_file.write("  int " + var_list + "\n")
        for level in levels_dict.keys():
            the_C_file.write("  " + level + ": " + levels_dict[level] + "\n")
        the_C_file.write("}\n")
    # written successfully
        


def program():
    if startingTk == programTk:
        unit, token, line = lex()
        program_name = unit # need this for intermediate code purposes
        
        final_code_lines.append("L_start:")
        final_code_lines.append("   b Lmain")
        if token == IDTk:
            unit, token, line = lex()
            # symbol matrix
            #______________
            global scopeCounter
            newScope = RecordScope(scopeCounter) 
            recordScopes.append(newScope)
            scopeCounter += 1
            #______________
            unit, token, line = block(unit, token, line, program_name, isSubProgram=False, isFunction=False, func_ent = None)
            if token != dotTk:
                print("Syntax error at line", line, ": program must end with a dot.")
                exit()
            elif symbol_counter < symbol_counter_limit:
                for i in range(symbol_counter, symbol_counter_limit):
                    if symbols[i] not in ['\n', ' ']:
                        print("Warning: Symbols found after the dot, which signals the end of the program. Everything after the dot will be ignored.")
                        break
            # symbol matrix
            #______________

            for scope in recordScopes:
                symbol_file.write(scope.printRecordScope() + "\n")
                symbol_file.write("\n")
            symbol_file.write("*********************")
            symbol_file.write("\n")
            symbol_file.write("\n")

            del recordScopes[-1]
            scopeCounter -= 1
            if scopeCounter > 0:
                recordScopes[scopeCounter - 1].setNextRecordScope(None)
            
            #______________
        else:
            print("Syntax error at line", line, ": program name was expected.")
            exit()
    else:
        print("Syntax error at line", startingLine, ": the keyword 'program' was expected, but '%s' was found." % startingUnit)
        exit()

def block(unit, token, line, name, isSubProgram, isFunction, func_ent):
    index = 0
    unit, token, line = declarations(unit, token, line )
    unit, token, line = subprograms(unit, token, line)
    genquad("begin_block", name, "_", "_")
    if not isSubProgram and not isFunction:
        final_code_lines.append("Lmain:")
        final_code_lines.append("L_" + str(quad_label) + ":")
        final_code_lines.append("   addi $sp,$sp,")
        index = len(final_code_lines) - 1 # keep the index of the above final code line in order to fill in the framelength when we calculate it
        final_code_lines.append("   move s0,$sp")
    elif isSubProgram:
        finalCodeLevels[func_ent.getEntityName()] =  "L_" + str(quad_label)
        final_code_lines.append("L_" + str(quad_label) + ":")
        final_code_lines.append("   sw $ra,($sp)")
    returnStatExists = False # for semantic analysis purposes
    unit, token, line, returnStatCount = statements(unit, token, line)
    if isFunction and returnStatCount < 1:
        print("Semantic analysis error at line", line, ": function without return statement.")
        exit()
    elif isSubProgram and not isFunction and returnStatCount > 0:
        print("Semantic analysis error at line", line, ": procedures can't have return statement.")
        exit()
    elif not isSubProgram and returnStatCount > 0:
        print("Semantic analysis error at line", line, ": return statement found outside function (not allowed).")
        exit()
    if not isSubProgram and not isFunction:
        framelength = 12
        for entity in recordScopes[0].getEntities():
            if isinstance(entity, VarEntity) or isinstance(entity, parEntity):
                framelength += 4
        final_code_lines[index] += str(framelength)
    if(not isSubProgram): # we need to create the halt quad only for the main block, not for subprograms
        genquad("halt", "_", "_", "_")
    genquad("end_block", name, "_", "_")
    return unit, token, line

def declarations(unit, token, line):
    while token == declareTk:
        
        unit, token, line = lex()
        variableLine = line
        unit, token, line = varlist(unit, token, line )
        if token == semicolonTk:
            unit, token, line = lex()
        else:
            print("Syntax error at line", variableLine, ": ';' expected at the end of the declared variables list, or maybe you failed to seperate the variables with commas.")
            exit()
    return unit, token, line 

def varlist(unit, token, line):
    if token == IDTk:
        C_variable_names.append(unit)
        # symbol matrix
        #______________
        global scopeCounter
        checkForRedeclaration(unit, scopeCounter - 1, line)
        varEntity = VarEntity(unit, 0)
        recordScopes[scopeCounter - 1].addEntity(varEntity)
        #______________
        unit, token, line = lex()
        while unit == ",":
            unit, token, line = lex()
            if token != IDTk:
                print("Syntax error at line", line, ": another ID name expected after ',' .")
                exit()
            else:
                C_variable_names.append(unit)
                # symbol matrix
                #______________
                checkForRedeclaration(unit, scopeCounter - 1, line)
                varEntity = VarEntity(unit, 0)
                recordScopes[scopeCounter - 1].addEntity(varEntity)
                #______________
                unit, token, line = lex()

    return unit, token, line 

def subprograms(unit, token, line):
    while token == functionTk or token == procedureTk:
        subprogramTk = token
        unit, token, line = lex()
        if subprogramTk == functionTk:
            unit, token, line = subprogram(unit, token, line, isFunction=True)
        else:
            unit, token, line = subprogram(unit, token, line, isFunction=False)
    return unit, token, line

def subprogram(unit, token, line, isFunction):
    boolValue = isFunction
    global let_C_code_be_created
    # symbol matrix
    #______________
    global scopeCounter
    newScope = RecordScope(scopeCounter) 
    recordScopes.append(newScope)
    scopeCounter += 1
    if scopeCounter > 1:
        recordScopes[scopeCounter - 2].setNextRecordScope(newScope)
    
    #______________
    let_C_code_be_created = False
    if token == IDTk:
        program_name = unit
        # symbol matrix
        #______________
        global quad_label
        funcEnt = funcEntity(program_name)
        checkForRedeclaration(program_name, scopeCounter - 2, line)
        recordScopes[scopeCounter - 2].addEntity(funcEnt, isFunc=True)
        previousLabel = quad_label
        #______________
        unit, token, line = lex()
        if token == leftParenthesisTk:
            unit, token, line = lex()
            unit, token, line = formalparlist(unit, token, line)
            if token == rightParenthesisTk:
                unit, token, line = lex()
                unit, token, line = block(unit, token, line, program_name, isSubProgram=True, isFunction=boolValue, func_ent = funcEnt)
                final_code_lines.append("L_" + str(quad_label) + ":")
                final_code_lines.append("   lw $ra,($sp)")
                final_code_lines.append("   jr $ra")
                # symbol matrix
                #______________
                if previousLabel != quad_label:
                    funcEnt.setStartQuad(previousLabel + 2)
                framelength = 12 # always 12 bytes at the start of each function..
                for entity in recordScopes[scopeCounter - 1].getEntities():
                    if isinstance(entity, VarEntity) or isinstance(entity, parEntity):
                        framelength += 4 #..and then 4 bytes for each varEnity/parEntity
                funcEnt.setFramelength(framelength)
                #______________
            else:
                print("Syntax error at line", line, ": unclosed parenthesis after parameters list.")
        else:
           print("Syntax error at line", line, ": left parenthesis opening expected after function/procedure ID (for parameters).")
    else:
        print("Syntax error at line", line, ": ID name expected after function/procedure keyword.")
        exit()
    # symbol matrix
    #______________
    for scope in recordScopes:
        symbol_file.write(scope.printRecordScope() + "\n")
        symbol_file.write("\n")
    symbol_file.write("*********************")
    symbol_file.write("\n")
    symbol_file.write("\n")

    del recordScopes[-1]
    scopeCounter -= 1
    if scopeCounter > 0:
        recordScopes[scopeCounter - 1].setNextRecordScope(None)
    
    #______________
    return unit, token, line

def formalparlist(unit, token, line):
    while token == inTk or token == inoutTk:
        unit, token, line = formalparitem(unit, token, line)
        
    return unit, token, line

def formalparitem(unit, token, line):
    initialUnit = unit # for knowing if it is an in or inout..
    # symbol matrix
    #______________
    global scopeCounter
    argument = Argument(unit)
    recordScopes[scopeCounter - 2].funcEntityAddArgument(argument)
    #______________
    unit, token, line = lex()
    id_unit = unit # for symbol matrix
    if token == IDTk:
        unit, token, line = lex()
        # symbol matrix
        #______________
        parEnt = parEntity(id_unit, initialUnit, 0)
        checkForRedeclaration(id_unit, scopeCounter - 1, line)
        recordScopes[scopeCounter - 1].addEntity(parEnt)
        #______________
        while token == commaTk:
            unit, token, line = lex()
            initialUnit = unit # for knowing if it is an in or inout..
            # symbol matrix
            #______________
            argument = Argument(unit)
            recordScopes[scopeCounter - 2].funcEntityAddArgument(argument)
            #______________
            if token != inTk and token != inoutTk:
                print("Syntax error at line", line, ": expected 'in' or 'inout' keyword after ','")
                exit()
            else:
                initialUnit = unit
                unit, token, line = lex()
                if token == IDTk:
                    # symbol matrix
                    #______________
                    parEnt = parEntity(unit, initialUnit, 0)
                    checkForRedeclaration(id_unit, scopeCounter - 1, line)
                    recordScopes[scopeCounter - 1].addEntity(parEnt)
                    #______________
                    unit, token, line = lex()
                else:
                    print("Syntax error at line", line, ": expected parameter name after '", initialUnit, "' keyword." )
                    exit()
    else:
        print("Syntax error at line", line, ": expected ID name after keyword", initialUnit, ".")
        exit()
    return unit, token, line

def statements(unit, token, line):
    returnStatCount = 0
    if token == leftBraceTk:
        unit, token, line = lex()
        unit, token, line, count = statement(unit, token, line)
        returnStatCount += count
        if token == semicolonTk:
            unit, token, line = lex()
            
            while token in [IDTk, ifTk, whileTk, switchcaseTk, forcaseTk, incaseTk, callTk, returnTk, inputTk, printTk, semicolonTk]:
                unit, token, line, count = statement(unit, token, line)   
                returnStatCount += count
                if token == semicolonTk:
                    unit, token, line = lex()
                elif token != rightBraceTk:
                    print("Syntax error at line", line, ": statement without semicolon can only be final statement inside the braces.")
                    exit()
        elif token in [IDTk, ifTk, whileTk, switchcaseTk, forcaseTk, incaseTk, callTk, returnTk, inputTk, printTk, semicolonTk]:
            print("Syntax error at line", line, ": statement without semicolon can only be final statement inside the braces.")
            exit()
        if token == rightBraceTk:
            unit, token, line = lex()
        else:
            print("Syntax error at line", line, ": no right brace found after brace opening or maybe you opened a left brace in an invalid place. Braces can only contain statements.")
            exit()
    else:
        
        unit, token, line, count = statement(unit, token, line)
        returnStatCount += count
        if token != semicolonTk:
            print("Syntax error at line", line, ": expected ';' after statement")
            exit()
        unit, token, line = lex()
    return unit, token, line, returnStatCount

def statement(unit, token, line):
    count = 0
    if token == IDTk:
        id_name = unit # for quad creation
        unit, token, line = lex()
        unit, token, line = assignStat(unit, token, line, id_name)
    elif token == ifTk:
        unit, token, line = lex()
        unit, token, line, c = ifStat(unit, token, line)
        count += c
    elif token == whileTk:
        unit, token, line = lex()
        unit, token, line, c = whileStat(unit, token, line)
        count += c
    elif token == switchcaseTk:
        unit, token, line = lex()
        unit, token, line, c = switchcaseStat(unit, token, line)
        count += c
    elif token == forcaseTk:
        unit, token, line = lex()
        unit, token, line, c = forcaseStat(unit, token, line)
        count += c
    elif token == incaseTk:
        unit, token, line = lex()
        unit, token, line, c = incaseStat(unit, token, line)
        count += c
    elif token == callTk:
        unit, token, line = lex()
        unit, token, line = callStat(unit, token, line)
    elif token == returnTk:
        count += 1
        unit, token, line = lex()
        unit, token, line = returnStat(unit, token, line)
    elif token == inputTk:
        unit, token, line = lex()
        unit, token, line = inputStat(unit, token, line)
    elif token == printTk:
        unit, token, line = lex()
        unit, token, line = printStat(unit, token, line)
    return unit, token, line, count

def assignStat(unit, token, line, id_name):
    if token != assignTk:
        print("Syntax error at line", line, ": expected ':=' for ID assignment.")
        exit()
    searchForDeclaration(id_name, "var", line) # when we assign a value to a variable, we must have declared the variable
    unit, token, line = lex()
    unit, token, line, E_place = expression(unit, token, line)
    genquad(":=", E_place, "_", id_name)
    final_code_lines.append("L_" + str(quad_label) + ":")
    loadvr(E_place, "$t1")
    storerv("$t1", id_name)
    return unit, token, line

def ifStat(unit, token, line):
    returnStatCount = 0
    if token != leftParenthesisTk:
        print("Syntax error at line", line, ": expected left parenthesis after 'if' keyword.")
        exit()
    unit, token, line = lex()
    unit, token, line, B_true, B_false = condition(unit, token, line)
    if token != rightParenthesisTk:
        print("Syntax error at line", line, ": no right parenthesis found after condition.")
        exit()
    unit, token, line = lex()
    backpatch(B_true, nextquad())
    unit, token, line, count = statements(unit, token, line)
    returnStatCount += count
    ifList = makelist(nextquad())
    genquad("jump", "_", "_", "_")
    backpatch(B_false, nextquad())
    unit, token, line, count = elsepart(unit, token, line)
    returnStatCount += count
    backpatch(ifList, nextquad())
    final_code_lines.append("L_" + str(quad_label) + ":")
    final_code_lines.append("   b L_" +  str(nextquad()))
    return unit, token, line, returnStatCount

def elsepart(unit, token, line):
    returnStatCount = 0
    if token == elseTk:
        unit, token, line = lex()
        unit, token, line, count = statements(unit, token, line)
        returnStatCount += count
    return unit, token, line, returnStatCount

def whileStat(unit, token, line):
    returnStatCount = 0
    Bquad = nextquad()
    if token != leftParenthesisTk:
        print("Syntax error at line", line, ": no left parenthesis found after 'while' keyword.")
        exit()
    unit, token, line = lex()
    unit, token, line, B_true, B_false = condition(unit, token, line)
    if token != rightParenthesisTk:
        print("Syntax error at line", line, ": no right parenthesis found after condition.")
        exit()
    unit, token, line = lex()
    backpatch(B_true, nextquad())
    unit, token, line, count = statements(unit, token, line)
    returnStatCount += count
    genquad("jump", "_", "_", Bquad)
    final_code_lines.append("L_" + str(quad_label) + ":")
    final_code_lines.append("   b L_" +  str(Bquad))
    backpatch(B_false, nextquad())
    return unit, token, line, returnStatCount

def switchcaseStat(unit, token, line):
    returnStatCount = 0
    exitlist = emptylist()
    while token == caseTk:
        unit, token, line = lex()
        if token != leftParenthesisTk:
            print("Syntax error at line", line, ": no left parenthesis found after condition.")
            exit()
        unit, token, line = lex()
        unit, token, line, cond_true, cond_false = condition(unit, token, line)
        if token != rightParenthesisTk:
            print("Syntax error at line", line, ": no right parenthesis found after condition.")
            exit()
        unit, token, line = lex()
        backpatch(cond_true, nextquad())
        unit, token, line, count = statements(unit, token, line)
        returnStatCount += count
        e = makelist(nextquad())
        genquad("jump", "_", "_", "_")
        final_code_lines.append("L_" + str(quad_label) + ":")
        final_code_lines.append("   b L_"+ str(nextquad()))
        exitlist = merge(exitlist, e)
        backpatch(cond_false, nextquad())
    if token != defaultTk:
        print("Syntax error at line", line, ": no 'default' keyword found in forcase statement.")
        exit()
    unit, token, line = lex()
    unit, token, line, count = statements(unit, token, line)
    returnStatCount += count
    backpatch(exitlist, nextquad())
    return unit, token, line, returnStatCount

def forcaseStat(unit, token, line):
    returnStatCount = 0
    p1Quad = nextquad()
    while token == caseTk:
        unit, token, line = lex()
        if token != leftParenthesisTk:
            print("Syntax error at line", line, ": no right left parenthesis found after condition.")
            exit()
        unit, token, line = lex()
        unit, token, line, cond_true, cond_false = condition(unit, token, line)
        if token != rightParenthesisTk:
            print("Syntax error at line", line, ": no right parenthesis found after condition.")
            exit()
        unit, token, line = lex()
        backpatch(cond_true, nextquad())
        unit, token, line, count = statements(unit, token, line)
        returnStatCount += count
        genquad("jump", "_", "_", p1Quad)
        final_code_lines.append("L_" + str(quad_label) + ":")
        final_code_lines.append("   b L_" +  p1Quad)
        backpatch(cond_false, nextquad())
    if token != defaultTk:
        print("Syntax error at line", line, ": no 'default' keyword found in forcase statement.")
        exit()
    unit, token, line = lex()
    unit, token, line, count = statements(unit, token, line)
    returnStatCount += count
    return unit, token, line, returnStatCount

def incaseStat(unit, token, line):
    returnStatCount = 0
    w = newtemp()
    # symbol matrix
    #______________
    global scopeCounter
    varEntity = VarEntity(w, 0)
    recordScopes[scopeCounter - 1].addEntity(varEntity)
    #______________
    C_variable_names.append(w)
    p1Quad = nextquad()
    genquad(":=", 1, "_", w)
    final_code_lines.append("L_" + str(quad_label) + ":")
    loadvr(1, "$t1")
    storerv("$t1", w)
    while token == caseTk:
        unit, token, line = lex()
        if token != leftParenthesisTk:
            print("Syntax error at line", line, ": no left parenthesis found after condition.")
            exit()
        unit, token, line = lex()
        unit, token, line, cond_true, cond_false = condition(unit, token, line)
        if token != rightParenthesisTk:
            print("Syntax error at line", line, ": no right parenthesis found after condition.")
            exit()
        unit, token, line = lex()
        backpatch(cond_true, nextquad())
        final_code_lines.append("L_" + str(quad_label) + ":")
        genquad(":=", 0, "_", w)
        loadvr(0, "$t1")
        storerv("$t1", w)
        unit, token, line, count = statements(unit, token, line)
        returnStatCount += count
        backpatch(cond_false, nextquad())
    genquad("=", w, 0, p1Quad)
    return unit, token, line, returnStatCount

def returnStat(unit, token, line):
    if token != leftParenthesisTk:
        print("Syntax error at line", line, ": no left parenthesis found after 'return' keyword.")
        exit()
    unit, token, line = lex()
    unit, token, line, E_place = expression(unit, token, line)
    if token != rightParenthesisTk:
        print("Syntax error at line", line, ": no right parenthesis found after condition.")
        exit()
    genquad("retv", E_place, "_", "_")
    final_code_lines.append("L_" + str(quad_label) + ":")
    loadvr(E_place, "$t1")
    final_code_lines.append("   lw $t0,-8($sp)")
    final_code_lines.append("   sw $t1,($t0)")
    unit, token, line = lex()
    return unit, token, line

def callStat(unit, token, line):
    global let_C_code_be_created
    let_C_code_be_created = False
    procedure_name = unit # for quad generation
    funcEnt, funcScope = searchForDeclaration(procedure_name, "func", line)
    funcArgs = funcEnt.getArgumentParmodes()
    if token != IDTk:
        print("Syntax error at line", line, ": expected identifier after 'call' keyword.")
        exit()
    unit, token, line = lex()
    if token != leftParenthesisTk:
        print("Syntax error at line", line, ": no left parenthesis found after identifier.")
        exit()
    unit, token, line = lex()
    unit, token, line = actualparlist(unit, token, line, funcArgs, funcScope, funcEnt.getFramelength())
    if token != rightParenthesisTk:
        print("Syntax error at line", line, ": no right parenthesis found after parameters list.")
        exit()
    genquad("call", procedure_name, "_", "_")
    if funcArgs != []:
        final_code_lines.append("L_" + str(quad_label) + ":")
    #final_code_lines.append("   addi $fp,$sp,12")
    if funcScope == scopeCounter:
        final_code_lines.append("   lw $t0,-4($sp)")
        final_code_lines.append("   sw $t0,-4($fp)")
    else:
        final_code_lines.append("   sw $sp,-4($fp)")
    final_code_lines.append("   addi $sp,$sp," + str(funcEnt.getFramelength()))
    final_code_lines.append("   jal " + finalCodeLevels[procedure_name])
    final_code_lines.append("   addi $sp,$sp,-" + str(funcEnt.getFramelength()))
    unit, token, line = lex()
    return unit, token, line

def printStat(unit, token, line):
    if token != leftParenthesisTk:
        print("Syntax error at line", line, ": no left parenthesis found after 'print' keyword.")
        exit()
    unit, token, line = lex()
    unit, token, line, E_place = expression(unit, token, line)
    if token != rightParenthesisTk:
        print("Syntax error at line", line, ": no right parenthesis found after expression.")
        exit()
    unit, token, line = lex()
    genquad("out", E_place, "_", "_")
    final_code_lines.append("L_" + str(quad_label) + ":")
    final_code_lines.append("   li $v0,1")
    loadvr(E_place, "$a0")
    final_code_lines.append("   syscall")
    return unit, token, line

def inputStat(unit, token, line):
    if token != leftParenthesisTk:
        print("Syntax error at line", line, ": no left parenthesis found after 'input' keyword.")
        exit()
    unit, token, line = lex()
    id_place = unit # for quad generation
    if token != IDTk:
        print("Syntax error at line", line, ": expected ID after left parenthesis on input statement.")
        exit()
    searchForDeclaration(id_place, "var", line) # if we input a value to a variable, the variable needs to have been declared
    unit, token, line = lex()
    if token != rightParenthesisTk:
        print("Syntax error at line", line, ": no right parenthesis found after identifier.")
        exit()
    unit, token, line = lex()
    genquad("inp", id_place, "_", "_")
    final_code_lines.append("L_" + str(quad_label) + ":")
    final_code_lines.append("   li $v0,5")
    final_code_lines.append("   syscall")
    storerv("$v0", id_place)
    return unit, token, line

def actualparlist(unit, token, line, args, f_scope, framelength):
    outer_par_quads = []
    passed_args_counter = 0 # counts how many arguments we have passed to the function/procedure inside our program
    final_code_lines.append("L_" + str(quad_label + 1) + ":")
    final_code_lines.append("   addi $fp,$sp," + str(framelength))
    while token == inTk or token == inoutTk:
        passed_args_counter += 1
        if passed_args_counter > len(args): # if we pass a different number of parameters than the number needed
            print("Semantic analysis error at line", line , ": expected", len(args) ,"parameters, but got more than that.")
            exit()
        unit, token, line, ret_quads = actualparitem(unit, token, line, args[passed_args_counter - 1], passed_args_counter, f_scope)
        outer_par_quads += ret_quads
        if token == commaTk:
            unit, token, line = lex()
    if passed_args_counter < len(args): # if we pass a different number of parameters than the number needed
        print("Semantic analysis error at line", line , ": expected", len(args) ,"parameters, but got", passed_args_counter, ".")
        exit()
    for quad in outer_par_quads:
        genquad(quad[0], quad[1], quad[2], quad[3])
    return unit, token, line

def condition(unit, token, line):
    B_true = []
    B_false = []
    unit, token, line, Q1_true, Q1_false = boolterm(unit, token, line)
    B_true = Q1_true
    B_false = Q1_false
    while token == orTk:
        backpatch(B_false, nextquad())
        unit, token, line = lex()
        unit, token, line, Q2_true, Q2_false = boolterm(unit, token, line)
        B_true = merge(B_true, Q2_true)
        B_false = Q2_false
    return unit, token, line, B_true, B_false

def boolterm(unit, token, line):
    Q_true = []
    Q_false = []
    unit, token, line, R1_true, R1_false = boolfactor(unit, token, line)
    Q_true = R1_true
    Q_false = R1_false
    while token == andTk:
        backpatch(Q_true, nextquad())
        unit, token, line = lex()
        unit, token, line, R2_true, R2_false = boolfactor(unit, token, line)
        Q_false = merge(Q_false, R2_false)
        Q_true = R2_true
    return unit, token, line, Q_true, Q_false

def boolfactor(unit, token, line):
    R_true = []
    R_false = []
    if token == notTk:
        unit, token, line = lex()
        if token != leftBracketTk:
            print("Syntax error at line", line, ": no left bracket found after 'not' keyword.")
            exit()
        unit, token, line = lex()
        unit, token, line, B_true, B_false = condition(unit, token, line)
        if token != rightBracketTk:
            print("Syntax error at line", line, ": no right bracket found after bracket opening.")
            exit()
        unit, token, line = lex()
        R_true = B_false
        R_false = B_true
    elif token == leftBracketTk:
        unit, token, line = lex()
        unit, token, line, B_true, B_false = condition(unit, token, line)
        if token != rightBracketTk:
            print("Syntax error at line", line, ": no right bracket found after bracket opening.")
            exit()
        unit, token, line = lex()
        R_true = B_true
        R_false = B_false
    else:
        unit, token, line, E1_place = expression(unit, token, line)
        relop = unit # needer for quad creation
        unit, token, line = REL_OP(unit, token, line)
        unit, token, line, E2_place = expression(unit, token, line)
        R_true = makelist(nextquad())
        genquad(relop, E1_place, E2_place, "_")
        final_code_lines.append("L_" + str(quad_label) + ":")
        loadvr(E1_place, "$t1")
        loadvr(E2_place, "$t2")
        if relop == "=":
            final_code_lines.append("   beq $t1,$t2,")
        if relop == "<>":
            final_code_lines.append("   bne $t1,$t2,")
        if relop == ">":
            final_code_lines.append("   bgt $t1,$t2,")
        if relop == "<":
            final_code_lines.append("   blt $t1,$t2,")
        if relop == ">=":
            final_code_lines.append("   bge $t1,$t2,")
        if relop == "<=":
            final_code_lines.append("   ble $t1,$t2,")
        R_false = makelist(nextquad())
        genquad("jump", "_", "_", "_")
        final_code_lines.append("L_" + str(quad_label) + ":")
        final_code_lines.append("   b L_" +  str(nextquad()))

    return unit, token, line, R_true, R_false
    
def expression(unit, token, line, ):
    T1_place = "" 
    temp = ""
    unit, token, line, minusSign = optionalSign(unit, token, line)
    unit, token, line, T1_place = term(unit, token, line, T1_place)
    if minusSign == True:
        temp = newtemp()
        # symbol matrix
        #______________
        global scopeCounter
        varEntity = VarEntity(temp, 0)
        recordScopes[scopeCounter - 1].addEntity(varEntity)
        #______________
        C_variable_names.append(temp)
        genquad("-", 0, T1_place, temp)
        T1_place = temp
    while token == plusTk or token == minusTk:
        initialToken = token
        unit, token, line = ADD_OP(unit, token, line)
        T2_place = ""
        unit, token, line, T2_place = term(unit, token, line, T2_place)
        w = newtemp()
        # symbol matrix
        #______________
        varEntity = VarEntity(w, 0)
        recordScopes[scopeCounter - 1].addEntity(varEntity)
        #______________
        C_variable_names.append(w)
        final_code_lines.append("L_" + str(quad_label + 1) + ":")
        loadvr(T1_place, "$t1");
        loadvr(T2_place, "$t2")
        if initialToken == plusTk:
            genquad("+", T1_place, T2_place, w)
            final_code_lines.append("   add $t1,$t1,$t2")
        elif initialToken == minusTk:
            genquad("-", T1_place, T2_place, w)
            final_code_lines.append("   sub $t1,$t1,$t2")
        storerv("$t1", w)
        T1_place = w 
    
    E_place = T1_place
    return unit, token, line, E_place

def term(unit, token, line, E_place):
    T1_place = ""
    unit, token, line, T1_place = factor(unit, token, line)
    while token == divideTk or token == multiplyTk:
        initialToken = token
        unit, token, line = MUL_OP(unit, token, line)
        T2_place = ""
        unit, token, line, T2_place = factor(unit, token, line)
        w = newtemp()
        # symbol matrix
        #______________
        global scopeCounter
        varEntity = VarEntity(w, 0)
        recordScopes[scopeCounter - 1].addEntity(varEntity)
        #______________
        C_variable_names.append(w)
        final_code_lines.append("L_" + str(quad_label + 1) + ":")
        loadvr(T1_place, "$t1")
        loadvr(T2_place, "$t2")
        if initialToken == divideTk:
            genquad("/", T1_place, T2_place, w)
            final_code_lines.append("   div $t1,$t1,$t2")
        elif initialToken == multiplyTk:
            genquad("*", T1_place, T2_place, w)
            final_code_lines.append("   mul $t1,$t1,$t2")
        storerv("$t1", w)
        T1_place = w 

    E_place = T1_place
    return unit, token, line, E_place

def factor(unit, token, line):
    F_place = ""
    initialUnit = unit # for error message purposes
    if token == leftParenthesisTk:
        unit, token, line = lex()
        unit, token, line, E_place = expression(unit, token, line)
        if token != rightParenthesisTk:
            print("Syntax error at line", line, ": no right parenthesis found after identifier.")
            exit()
        unit, token, line = lex()
        F_place = E_place
    elif token == IDTk:
        id_place = unit
        unit, token, line = lex()
        unit, token, line, id_place = idtail(unit, token, line, id_place) # pass the id because we have an idtail only when we call a function, so we need to create the "call" quad
        F_place = id_place
    elif token!= integerValueTk:
        print("Syntax error at line", line, ": expected integer/(expression)/identifier as factor instead of '", initialUnit, "' .")
        exit()
    elif token == integerValueTk:
        F_place = unit
        unit, token, line = lex()
    return unit, token, line, F_place

def idtail(unit, token, line, id_place):
    if token == leftParenthesisTk:
        funcEnt, funcScope = searchForDeclaration(id_place, "func", line) # since we have idtail, it means we call a function, so we need to check if the function has been declared
        funcArgs = funcEnt.getArgumentParmodes()
        global let_C_code_be_created
        let_C_code_be_created = False # We enter here only when we call a function, which means, there is a chance we haven't defined it but we call it (semantic error).
        unit, token, line = lex()
        unit, token, line = actualparlist(unit, token, line, funcArgs, funcScope, funcEnt.getFramelength())
        if token != rightParenthesisTk:
            print("Syntax error at line", line, ": no right parenthesis found after parameter list.")
            exit()
        unit, token, line = lex()

        # If we got here, it means that we are calling a function, so we need to create the quads for it after the pars have been named.
        w = newtemp()
        # symbol matrix
        #______________
        global scopeCounter
        varEntity = VarEntity(w, 0)
        recordScopes[scopeCounter - 1].addEntity(varEntity)
        #______________
        C_variable_names.append(w)
        genquad("par", w, "RET", "_")
        final_code_lines.append("L_" + str(quad_label) + ":")
        final_code_lines.append("   addi $t0,$sp,-" + str(varEntity.getOffset()))
        final_code_lines.append("   sw $t0,-8($fp)")
        genquad("call", "_", "_", id_place)
        final_code_lines.append("L_" + str(quad_label) + ":")
        if funcScope == scopeCounter:
            final_code_lines.append("   lw $t0,-4($sp)")
            final_code_lines.append("   sw $t0,-4($fp)")
        else:
            final_code_lines.append("   sw $sp,-4($fp)")
        final_code_lines.append("   addi $sp,$sp," + str(funcEnt.getFramelength()))
        final_code_lines.append("   jal " + finalCodeLevels[id_place])
        final_code_lines.append("   addi $sp,$sp,-" + str(funcEnt.getFramelength()))
        id_place = w
    else:
        # if we get here, we use a variable ID in an expression, so we need to check if we have declared it
        searchForDeclaration(id_place, "var", line)
    return unit, token, line, id_place

def optionalSign(unit, token, line):
    minusSign = False
    if token == plusTk or token == minusTk:
        if token == minusTk:
            minusSign = True
        unit, token, line = ADD_OP(unit, token, line)
    return unit, token, line, minusSign

def REL_OP(unit, token, line):
    if token == equalToTk or token == lesserOrEqualToTk or token == greaterOrEqualToTk or token == greaterThanTk or token == lesserThanTk or token == greaterOrLesserThanTk:
        unit, token, line = lex()
        return unit, token, line
    print("Syntax error at line", line, ": expected relational operator instead of", unit,".")
    exit()

def ADD_OP(unit, token, line):
    unit, token, line = lex()
    return unit, token, line

def MUL_OP(unit, token, line):
    unit, token, line = lex()
    return unit, token, line

def actualparitem(unit, token, line, passedPar, indexOfPar, funcScope):
    outer_par_quads = [] # If the parameter expression is a call of another function/procedure, then we need to store the pars of the outer function until the pars are finished.
    if token == inTk:
        if passedPar != "in":
            print("Semantic analysis error in line", line, ": parameter number", indexOfPar, "must be passed by reference, not by value.")
            exit()
        unit, token, line = lex()
        unit, token, line, E_place = expression(unit, token, line)
        # genquad("par", E_place, "CV", "_")
        outer_par_quads.append(["par", E_place, "CV", "_"])
        if indexOfPar > 1:
            final_code_lines.append("L_" + str(quad_label + indexOfPar) + ":")
        loadvr(E_place, "$t0")
        offset = - (12 + 4 * (indexOfPar - 1))
        final_code_lines.append("   sw $t0," + str(offset) + "($fp)")
    elif token == inoutTk:
        if passedPar != "inout":
            print("Semantic analysis error in line", line, ": parameter number", indexOfPar, "must be passed by value, not by reference.")
            exit()
        unit, token, line = lex()
        parName = unit # for quad generation
        if token != IDTk:
            print("Syntax error at line", line, ": ID expected after 'inout' keyword, but got", unit," instead.")
            exit()
        ent, unused = searchForDeclaration(parName, "var", line) # if we pass a variable as parameter, we need to have declared the variable
        depth = findParDepth(parName, funcScope)
        if indexOfPar > 1:
            final_code_lines.append("L_" + str(quad_label + indexOfPar) + ":")
        if depth == "local":
            offset = - (12 + 4 * (indexOfPar - 1))
            final_code_lines.append("   addi $t0,$sp,-" + str(ent.getOffset()))
            final_code_lines.append("   sw $t0,"+ str(offset) +"($fp)")
        elif depth == "parameter_by_reference":
            final_code_lines.append("   lw $t0,-" + str(ent.getOffset()) + "($sp)")
            final_code_lines.append("   sw $t0,"+ str(offset) +"($fp)")
        elif depth == "ancestor":
            gnvlcode(ent.getName())
            final_code_lines.append("   sw $t0,"+ str(offset) +"($fp)")
        elif depth == "ancestor_by_reference":
            gnvlcode(ent.getName())
            final_code_lines.append("   lw $t0,($t0)")
            final_code_lines.append("   sw $t0,"+ str(offset) +"($fp)")

        # genquad("par", parName, "REF", "_")
        outer_par_quads.append(["par", parName, "REF", "_"])
        unit, token, line = lex()
    return unit, token, line, outer_par_quads
        

syntaxAnalysisAndIntCodeCreation()

if let_C_code_be_created == True:
    create_C_code()

