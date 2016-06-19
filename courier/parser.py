
import ipdb
import core
import lexer
from lexer import TokenType
from typelib import datatypes
from typelib import utils
from typelib import fields

class UnexpectedTokenException(Exception):
    def __init__(self, found_token, *expected_tokens):
        message = "Line %d:%d - Token encountered '%s', " % (found_token.line, found_token.col, found_token.value)
        if len(expected_tokens) == 1:
            message += "Expected: %s" % expected_tokens[0]
        else:
            message += "Expected one of (%s)" % ", ".join(["'%s'" % tok.value for tok in expected_tokens])
        Exception.__init__(self, message)

class Parser(object):
    """
    Parses a Courier compilation unit and extracts all records define in it.
    """
    def __init__(self, instream, registry):
        self.peeked_tokens = []
        self.lexer = lexer.Lexer(instream)
        self.document = core.Document()
        self.imports = set()
        self._last_docstring = ""
        self.type_registry = registry

    def get_type(self, fqn):
        """
        Get's the type corresponding to the given fqn if it exists
        otherwise returns an unresolved type as a place holder until
        it can be resolved.
        """
        t = self.type_registry.get_type(fqn, nothrow = True)
        if not t:
            # TODO: if the fqn is actually NOT fully qualified, then
            # see if this matches any of the ones in the import decls

            # Try with the namespace as well
            n,ns,fqn = utils.normalize_name_and_ns(fqn, self.document.namespace, ensure_namespaces_are_equal=False)
            t = self.type_registry.get_type(fqn, nothrow = True)
            if not t:
                t = datatypes.Type(None, n, ns)
                t = self.type_registry.register_type(t)
        return t

    def register_type(self, newtype):
        """
        Registers a given type (by its fqn).  If the type already exists 
        and it is unresolved, then existing type is resolved with the new 
        type.  Otherwise a DuplicateException is thrown.
        """
        curr_type = self.type_registry.get_type(newtype.fqn, nothrow = True)
        if curr_type == None:
            return self.type_registry.register_type(newtype)
        elif curr_type.is_unresolved:
            curr_type.copy_from(newtype)
            return curr_type
        else:
            raise Exception("'%s' is already defined" % newtype.fqn)

    def add_import(self, fqn):
        self.imports.add(fqn)

    def last_docstring(self, reset = True):
        out = self._last_docstring
        if reset:
            self._last_docstring = ""
        return out

    def reset_last_docstring(self):
        self._last_docstring = ""

    def unget_token(self, token):
        self.peeked_tokens.append(token)

    def peek_token(self):
        return self.next_token(True)

    def next_token(self, peek = False):
        if not self.peeked_tokens:
            self.peeked_tokens.append(self.lexer.next())
        out = self.peeked_tokens[-1]
        if not peek:
            self.peeked_tokens.pop()
        return out

    def next_token_if(self, tok_type, tok_value = None, consume = False, ignore_comment = True):
        """
        Returns the next token if it matches a particular type and value otherwise returns None.
        """
        next_tok = self.peek_token()
        while next_tok.tok_type == TokenType.COMMENT:
            # Save the last docstring encountered as it can
            # be used as a docstring for the next entity that
            # needs it.
            self.last_docstring = next_tok.value
            if ignore_comment:
                self.next_token()
                next_tok = self.peek_token()
            else:
                # dont ignore comment so break out to be treated
                # as a real token
                break

        if next_tok.tok_type != tok_type:
            return None

        if tok_value is not None and next_tok.value != tok_value:
            return None

        if consume:
            self.next_token()
        return next_tok

    def next_token_is(self, tok_type, tok_value = None, consume = False, ignore_comment = True):
        """
        Returns true if the next token matches the given token type and the value if it is specified.
        Also if consume is True the token is consumed.
        """
        next_tok = self.next_token_if(tok_type, tok_value, consume, ignore_comment)
        return next_tok is not None

    def ensure_token(self, tok_type, tok_value = None, peek = False):
        """
        If the next token is not of the given type an exception is raised
        otherwise the token's value is returned.  Note that a token's value
        can be None and this is still valid.
        """
        if not self.next_token_is(tok_type, tok_value):
            raise UnexpectedTokenException(self.peek_token(), tok_type)
        if peek:
            return self.peek_token().value
        else:
            return self.next_token().value

    def parse(self):
        parse_compilation_unit(self)

    def ensure_leaf_value(self):
        if self.next_token_is(TokenType.NUMBER):
            # TODO: handle long vs int vs float etc
            return self.ensure_token(TokenType.NUMBER)
        elif self.next_token_is(TokenType.STRING):
            return self.ensure_token(TokenType.STRING)
        elif self.next_token_is(TokenType.IDENTIFIER):
            return self.ensure_fqn()
        else:
            raise UnexpectedTokenException(self.peek_token(), TokenType.STRING, TokenType.NUMBER, TokenType.IDENTIFIER)

    def ensure_fqn(self, delim_token = None):
        """
        Expects and parses a fully qualified name defined as:

            IDENT ( <delim> IDENT ) *
        """
        delim_token = delim_token or TokenType.DOT
        fqn = self.ensure_token(TokenType.IDENTIFIER)

        # no comments inside FQNs
        self.lexer.comments_enabled = False
        while self.next_token_is(delim_token):
            tok = self.next_token()
            if self.next_token_is(TokenType.IDENTIFIER):
                fqn += "." + self.next_token().value
            else:
                self.unget_token(tok)
                break
        self.lexer.comments_enabled = True
        return fqn

    def ensure_field_path(self):
        """
        Reads a field_path and returns the source record name as well as the 
        referenced field path nested within it.
        """
        source_record_name = self.ensure_fqn()
        field_path = None
        self.lexer.comments_enabled = False
        nt1 = self.next_token_if(TokenType.SLASH, consume = True)
        self.lexer.comments_enabled = True
        if nt1 is not None:
            if self.next_token_is(TokenType.IDENTIFIER):
                field_path = self.ensure_fqn(TokenType.SLASH)
            else:
                self.unget_token(nt1)
        return source_record_name, field_path

########################################################################
##          Production rules
########################################################################

def parse_compilation_unit(parser):
    """
    Parses a Courier compilation unit.

    compilation_unit := 
        NAMESPACE_DECL ?

        (IMPORT_DECL | TYPE_DECL) *
    """
    parse_namespace(parser)
    while parse_declaration(parser): pass

def parse_namespace(parser):
    """
    Parse the namespace for the current document.
    """
    if parser.next_token_is(TokenType.KW_NAMESPACE, consume = True):
        parser.document.namespace = parser.ensure_fqn()

def parse_declaration(parser):
    """
    Parse the declarations for the current document:

        declaration := import_statement | type_declaration
    """
    try:
        next = parser.peek_token()
    except StopIteration, si:
        # we are done
        return False
    if next.tok_type == TokenType.KW_IMPORT:
        parse_import_decl(parser)
    else:
        parse_type_decl(parser)
    return True

def parse_import_decl(parser):
    """
    Parse import declarations of the form below and adds it to the current document.

        import IDENTIFIER ( "." IDENTIFIER ) *
    """
    parser.ensure_token(TokenType.KW_IMPORT)
    fqn = parser.ensure_fqn()
    parser.add_import(fqn)
    return fqn

def parse_type_decl(parser):
    """
    Parses top level type declarations:

        type_declaration := annotation * ( typeref_decl | complex_type_decl)
    """
    annotations = parse_annotations(parser)
    type_class = parser.ensure_token(TokenType.IDENTIFIER, peek = True)
    if type_class == "typeref":
        return parse_typeref_decl(parser, annotations)
    else:
        # read the concrete type
        return parse_complex_type_decl(parser, annotations)

def parse_typeref_decl(parser, annotations):
    """
    Parses typeref declaration of the form:

        "typeref" <name> "=" type_data
    """
    parser.ensure_token(TokenType.IDENTIFIER, "typeref")
    name = parser.ensure_token(TokenType.IDENTIFIER)
    type_decl = core.TypeDeclaration(name, parser.last_docstring(), annotations)
    parser.ensure_token(TokenType.EQUALS)
    target_type = parse_any_type_decl(parser)

    # What should the type decl be here?
    name, namespace, fqn = utils.normalize_name_and_ns(name, parser.document.namespace)
    newtype = datatypes.AliasType(name, namespace, target_type)
    newtype = parser.register_type(newtype)
    return newtype

def parse_any_type_decl(parser, annotations = []):
    next_token = parser.ensure_token(TokenType.IDENTIFIER, peek = True)
    if next_token == "array":
        return parse_array_type_decl(parser, annotations)
    elif next_token == "map":
        return parse_map_type_decl(parser, annotations)
    elif next_token in [ "record", "enum", "union" ]:
        return parse_complex_type_decl(parser, annotations)
    else:
        return parse_primitive_type(parser, annotations)

def parse_primitive_type(parser, annotations = []):
    parser.ensure_token(TokenType.IDENTIFIER, peek = True)
    fqn = parser.ensure_fqn()
    # if this type exists in the type registry use this type
    # otherwise register as an unresolved type and proceed
    return parser.get_type(fqn)

def parse_array_type_decl(parser, annotations = []):
    parser.ensure_token(TokenType.IDENTIFIER, "array")
    parser.ensure_token(TokenType.OPEN_SQUARE)
    target_type = parse_any_type_decl(parser)
    parser.ensure_token(TokenType.CLOSE_SQUARE)
    return datatypes.ListType(target_type)

def parse_map_type_decl(parser, annotations = []):
    parser.ensure_token(TokenType.IDENTIFIER, "map")
    parser.ensure_token(TokenType.OPEN_SQUARE)
    key_type = parse_any_type_decl(parser)
    parser.ensure_token(TokenType.COMMA)
    value_type = parse_any_type_decl(parser)
    parser.ensure_token(TokenType.CLOSE_SQUARE)
    return datatypes.MapType(key_type, value_type)

def parse_complex_type_decl(parser, annotations = []):
    type_class = parser.ensure_token(TokenType.IDENTIFIER)
    if type_class not in ["union", "enum", "record"]:
        raise UnexpectedTokenException(parser.peek_token(), "union", "enum", "record")

    name = None
    if parser.next_token_is(TokenType.IDENTIFIER):
        # we have a name
        name = parser.ensure_token(TokenType.IDENTIFIER)

    if type_class == "enum":
        newtype = datatypes.EnumType(name, parser.document.namespace)
        newtype.type_data.annotations = annotations
        if name: 
            newtype = parser.register_type(newtype)
        parse_enum_body(parser, newtype)
    elif type_class == "union":
        newtype = datatypes.UnionType(name, parser.document.namespace)
        newtype.type_data.annotations = annotations
        if name: 
            newtype = parser.register_type(newtype)
        parse_union_body(parser, newtype)
    else:
        newtype = datatypes.RecordType(name, parser.document.namespace)
        record_data = newtype.type_data
        record_data.annotations = annotations
        if name:
            newtype = parser.register_type(newtype)

        # go through the annotations and see if any sources need to be marked as dependant
        for annot in annotations:
            if annot.fqn == "sources" and isinstance(annot, datatypes.CompoundAnnotation):
                for param_spec in annot.param_specs:
                    type_alias, type_fqn = param_spec
                    record_data.add_source_record(parser.get_type(type_fqn), type_alias)
        parse_record_body(parser, newtype)

        # now mark a dependency between sources
        if record_data.source_types:
            source_types = [y for (x,y) in record_data.source_types]
            parser.type_registry.on_resolution(source_types, record_data)

    assert newtype is not None, "A type was NOT parsed"
    # if newtype.fqn: newtype = parser.register_type(newtype)
    return newtype

def parse_enum_body(parser, enum_type, named = True):
    """
    Parse the body of an enum declaration:

        "{" enum_symbols + "}"
        import IDENT ( "." IDENT ) *
    """
    parser.ensure_token(TokenType.OPEN_BRACE)
    while not parser.next_token_is(TokenType.CLOSE_BRACE):
        parse_enum_symbol(parser, enum_type)
    parser.ensure_token(TokenType.CLOSE_BRACE)
    return enum_type

def parse_enum_symbol(parser, enum_type):
    """
    Parses an enum symbol:

        annotations symbol_name
    """
    annotations = parse_annotations(parser)
    symbol = parser.ensure_token(TokenType.IDENTIFIER)
    enum_type.type_data.add_symbol(symbol, annotations, parser.last_docstring)

def parse_union_body(parser, union_type):
    """
    Parse the body of an union declaration:
        "{" any_type_decl + "}"
    """
    parser.ensure_token(TokenType.OPEN_SQUARE)
    while True:
        child_type = parse_any_type_decl(parser)
        union_type.type_data.add_type(child_type)
        if parser.next_token_is(TokenType.CLOSE_SQUARE):
            break
        parser.ensure_token(TokenType.COMMA)
    parser.ensure_token(TokenType.CLOSE_SQUARE)
    return union_type

########################################################################
##          Record and Field parsing
########################################################################
def parse_record_body(parser, record_type):
    """
    Parses the body of a record declaration:

        "{"
            ( field_decl | select_decls | drop_decls ) *
        "}"

        field_decl := annotation * IDENT ":" type "?"
    """
    parser.ensure_token(TokenType.OPEN_BRACE)

    # read annotations as they can be used by ... or field decls
    while not parser.next_token_is(TokenType.CLOSE_BRACE):
        annotations = parse_annotations(parser)
        if parser.next_token_is(TokenType.IDENTIFIER):
            parse_field_decl(parser, record_type, annotations)
        elif parser.next_token_is(TokenType.DOTDOTDOT, consume = True):
            parse_include_decl_body(parser, record_type, annotations)
        else:
            raise UnexpectedTokenException(parser.peek_token(), TokenType.DOTDOTDOT, TokenType.IDENTIFIER)

    parser.ensure_token(TokenType.CLOSE_BRACE)
    return record_type

def parse_field_decl(parser, record_type, annotations):
    field_name = parser.ensure_token(TokenType.IDENTIFIER)
    parser.ensure_token(TokenType.COLON)
    field_type = parse_any_type_decl(parser)
    field_is_optional = False
    if parser.next_token_is(TokenType.QUESTION, consume = True):
        field_is_optional = True
    field = fields.Field(field_name, None, field_type, record_type,
                         optional = field_is_optional, default = None, docs = parser.last_docstring, annotations = annotations)
    record_type.type_data.add_field(field)

def parse_include_decl_body(parser, record_type, annotations):
    """
    Parse the body of an include declaration (ie the bits after "..."):

        # Insert the field at the particular field path into the current context
        # It is an error if this is a "top" level path as it wont have a field name
        included_field_decl := 
                    field_path_decl
                |   field_path_decl "as" new_field_name
                |   field_path_decl "as" ":" new_field_type
                |   field_path_decl "as" new_field_name ":" new_field_type
                |   field_path_decl "/" ( "*" | "{" "}" | "{" field_selections "}" )  ( "with" "{" field_mapping * "}" ) ?
                |   "{" included_field_decl * "}"

        field_selections := field_selection ( "," field_selection ) *
        field_selection := field_name ( "as" new_field_name )
        field_mapping := FQN
    """
    annotations = annotations or []
    annotations.extend(parse_annotations(parser))

    source_record_name, field_path = parser.ensure_field_path()
    field_include = fields.FieldInclude(source_record_name, field_path, annotations)

    if parser.next_token_is(TokenType.KW_AS, consume = True):
        # We have a field renaming/retyping
        field_include.renamed_as = parser.next_token_if(TokenType.IDENTIFIER, consume = True)
        if parser.next_token_is(TokenType.COLON, consume = True):
            field_include.retyped_as = parse_any_type_decl(parser)

    elif parser.next_token_is(TokenType.SLASH, consume = True):
        # Inclusion of multiple fields - "/" has now been consumed
        if parser.next_token_is(TokenType.STAR, consume = True):
            # include "all" fields
            field_include.select_all_fields = True
        else:
            parser.ensure_token(TokenType.OPEN_BRACE)
            field_include.selectors = []
            while not parser.next_token_is(TokenType.CLOSE_BRACE, consume = True):
                field_name = parser.ensure_token(TokenType.IDENTIFIER)
                field_renamed_as = field_name
                if parser.next_token_is(TokenType.KW_AS, consume = True):
                    field_renamed_as = parser.ensure_token(TokenType.IDENTIFIER)
                field_include.selectors.append((field_name, field_renamed_as))

                if not parser.next_token_is(TokenType.COMMA, consume = True):
                    parser.ensure_token(TokenType.CLOSE_BRACE)
                    break
    record_type.type_data.add_field_include_spec(field_include)
    return field_include

########################################################################
##          Annotation Parsing
########################################################################
def parse_annotations(parser):
    """
    Parse a list of annotations
    """
    out = []
    while parser.next_token_is(TokenType.AT):
        out.append(parse_annotation(parser))
    return out

def parse_annotation(parser):
    """
    Parse an annotation:
        annotation :=   leaf_annotation     |
                        compound_annotation

        leaf_annotation := "@" FQN "=" ( NUMBER | STRING )
        compound_annotaiton := "@" FQN "(" parameters ")"
        parameter_expressions := FQN | FQN "=" ( NUMBER, STRING, FQN )
    """
    parser.ensure_token(TokenType.AT)
    fqn = parser.ensure_fqn()
    if parser.next_token_is(TokenType.EQUALS):
        return parse_leaf_annotation_body(parser, fqn)
    elif parser.next_token_is(TokenType.OPEN_PAREN):
        return parse_compound_annotation_body(parser, fqn)
    else:
        return datatypes.SimpleAnnotation(fqn)

def parse_leaf_annotation_body(parser, fqn):
    """
    Parses leaf annotations of the form:

        "=" value
    """
    parser.ensure_token(TokenType.EQUALS)
    value = parser.ensure_leaf_value()
    return datatypes.PropertyAnnotation(fqn, value)

def parse_compound_annotation_body(parser, fqn):
    """
    Parses compound annotation body of the form:

        "(" ( param_spec ( "," param_spec ) * ) ? ")"
        param_spec := name ( "=" value ) ?
    """
    param_specs = []
    parser.ensure_token(TokenType.OPEN_PAREN)
    while parser.next_token_is(TokenType.IDENTIFIER):
        param_name = parser.ensure_fqn()
        param_value = None
        if parser.next_token_is(TokenType.EQUALS, consume = True):
            param_value = parser.ensure_leaf_value()
            param_specs.append((param_name, param_value))
        else:
            param_specs.append((None, param_name))

        if parser.next_token_is(TokenType.COMMA, consume = True):
            assert parser.next_token_is(TokenType.IDENTIFIER)

    parser.ensure_token(TokenType.CLOSE_PAREN)
    return datatypes.CompoundAnnotation(fqn, param_specs)
