/*
 * Copyright 2014 Google Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// independent from idl_parser, since this code is not needed for most clients

#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/util.h"

namespace flatbuffers {

// Pedantic warning free version of toupper().
inline char ToUpper(char c) {
    return static_cast<char>(::toupper(c));
}

static std::string GeneratedFileName(const std::string& path,
                                     const std::string& file_name) {
    return path + file_name + "_generated.h";
}

namespace cpp {
class LmiCppGenerator : public BaseGenerator {
  public:
    LmiCppGenerator(const Parser& parser, const std::string& path,
                    const std::string& file_name)
        : BaseGenerator(parser, path, file_name, "", "::"),
          cur_name_space_(nullptr) {
        const char* keywords[] = { "alignas",
                                   "alignof",
                                   "and",
                                   "and_eq",
                                   "asm",
                                   "atomic_cancel",
                                   "atomic_commit",
                                   "atomic_noexcept",
                                   "auto",
                                   "bitand",
                                   "bitor",
                                   "bool",
                                   "break",
                                   "case",
                                   "catch",
                                   "char",
                                   "char16_t",
                                   "char32_t",
                                   "class",
                                   "compl",
                                   "concept",
                                   "const",
                                   "constexpr",
                                   "const_cast",
                                   "continue",
                                   "co_await",
                                   "co_return",
                                   "co_yield",
                                   "decltype",
                                   "default",
                                   "delete",
                                   "do",
                                   "double",
                                   "dynamic_cast",
                                   "else",
                                   "enum",
                                   "explicit",
                                   "export",
                                   "extern",
                                   "false",
                                   "float",
                                   "for",
                                   "friend",
                                   "goto",
                                   "if",
                                   "import",
                                   "inline",
                                   "int",
                                   "long",
                                   "module",
                                   "mutable",
                                   "namespace",
                                   "new",
                                   "noexcept",
                                   "not",
                                   "not_eq",
                                   "nullptr",
                                   "operator",
                                   "or",
                                   "or_eq",
                                   "private",
                                   "protected",
                                   "public",
                                   "register",
                                   "reinterpret_cast",
                                   "requires",
                                   "return",
                                   "short",
                                   "signed",
                                   "sizeof",
                                   "static",
                                   "static_assert",
                                   "static_cast",
                                   "struct",
                                   "switch",
                                   "synchronized",
                                   "template",
                                   "this",
                                   "thread_local",
                                   "throw",
                                   "true",
                                   "try",
                                   "typedef",
                                   "typeid",
                                   "typename",
                                   "union",
                                   "unsigned",
                                   "using",
                                   "virtual",
                                   "void",
                                   "volatile",
                                   "wchar_t",
                                   "while",
                                   "xor",
                                   "xor_eq",
                                   nullptr
                                 };

        for (auto kw = keywords; *kw; kw++) {
            keywords_.insert(*kw);
        }
    }

    std::string GenIncludeGuard() const {
        // Generate include guard.
        std::string guard = file_name_;
        // Remove any non-alpha-numeric characters that may appear in a filename.
        struct IsAlnum {
            bool operator()(char c) {
                return !isalnum(c);
            }
        };
        guard.erase(std::remove_if(guard.begin(), guard.end(), IsAlnum()),
                    guard.end());
        guard = "FLATBUFFERS_GENERATED_" + guard;
        guard += "_";
        // For further uniqueness, also add the namespace.
        auto name_space = parser_.current_namespace_;

        for (auto it = name_space->components.begin();
                it != name_space->components.end(); ++it) {
            guard += *it + "_";
        }

        guard += "H_";
        std::transform(guard.begin(), guard.end(), guard.begin(), ToUpper);
        return guard;
    }

    void GenIncludeDependencies() {
        int num_includes = 0;

        for (auto it = parser_.native_included_files_.begin();
                it != parser_.native_included_files_.end(); ++it) {
            code_ += "#include \"" + *it + "\"";
            num_includes++;
        }

        for (auto it = parser_.included_files_.begin();
                it != parser_.included_files_.end(); ++it) {
            if (it->second.empty()) {
                continue;
            }

            auto noext = flatbuffers::StripExtension(it->second);
            auto basename = flatbuffers::StripPath(noext);
            code_ += "#include \"" + parser_.opts.include_prefix +
                     (parser_.opts.keep_include_path ? noext : basename) +
                     "_generated.h\"";
            num_includes++;
        }

        if (num_includes) {
            code_ += "";
        }
    }

    std::string EscapeKeyword(const std::string& name) const {
        return keywords_.find(name) == keywords_.end() ? name : name + "_";
    }

    std::string Name(const Definition& def) const {
        return EscapeKeyword(def.name);
    }

    std::string Name(const EnumVal& ev) const {
        return EscapeKeyword(ev.name);
    }

    // Iterate through all definitions we haven't generate code for (enums,
    // structs, and tables) and output them to a single file.
    bool generate() {
        code_.Clear();
        code_ += "// " + std::string(FlatBuffersGeneratedWarning()) + "\n\n";
        const auto include_guard = GenIncludeGuard();
        code_ += "#ifndef " + include_guard;
        code_ += "#define " + include_guard;
        code_ += "";

        if (parser_.opts.gen_nullable) {
            code_ += "#pragma clang system_header\n\n";
        }

        code_ += "#include \"flatbuffers/flatbuffers.h\"";

        if (parser_.uses_flexbuffers_) {
            code_ += "#include \"flatbuffers/flexbuffers.h\"";
        }

        code_ += "";

        if (parser_.opts.include_dependence_headers) {
            GenIncludeDependencies();
        }

        assert(!cur_name_space_);

        // Generate forward declarations for all structs/tables, since they may
        // have circular references.
        for (auto it = parser_.structs_.vec.begin();
                it != parser_.structs_.vec.end(); ++it) {
            const auto& struct_def = **it;

            if (!struct_def.generated) {
                SetNameSpace(struct_def.defined_namespace);
                code_ += "struct " + Name(struct_def) + ";";
            }
        }

        code_ += "";

        // Generate code for all the enum declarations.
        for (auto it = parser_.enums_.vec.begin(); it != parser_.enums_.vec.end(); ++it) {
            const auto& enum_def = **it;
            if (!enum_def.generated) {
                SetNameSpace(enum_def.defined_namespace);
                GenEnum(enum_def);
            }
        }

        // Generate structs
        for (auto it = parser_.structs_.vec.begin(); it != parser_.structs_.vec.end(); ++it) {
            const auto& struct_def = **it;
            if (struct_def.fixed && !struct_def.generated) {
                SetNameSpace(struct_def.defined_namespace);
                GenStruct(struct_def);
            }
        }

        // Generate tables
        for (auto it = parser_.structs_.vec.begin(); it != parser_.structs_.vec.end(); ++it) {
            const auto& struct_def = **it;
            if (!struct_def.fixed && !struct_def.generated) {
                SetNameSpace(struct_def.defined_namespace);
                GenTable(struct_def);
            }
        }

        if (cur_name_space_) {
            SetNameSpace(nullptr);
        }

        // Close the include guard.
        code_ += "#endif  // " + include_guard;
        const auto file_path = GeneratedFileName(path_, file_name_);
        const auto final_code = code_.ToString();
        return SaveFile(file_path.c_str(), final_code, false);
    }

  private:
    CodeWriter code_;

    std::set<std::string> keywords_;

    // This tracks the current namespace so we can insert namespace declarations.
    const Namespace* cur_name_space_;

    const Namespace* CurrentNameSpace() const {
        return cur_name_space_;
    }

    // Translates a qualified name in flatbuffer text format to the same name in
    // the equivalent C++ namespace.
    static std::string TranslateNameSpace(const std::string& qualified_name) {
        std::string cpp_qualified_name = qualified_name;
        size_t start_pos = 0;

        while ((start_pos = cpp_qualified_name.find(".", start_pos)) !=
                std::string::npos) {
            cpp_qualified_name.replace(start_pos, 1, "::");
        }

        return cpp_qualified_name;
    }

    void GenComment(const std::vector<std::string>& dc, const char* prefix = "") {
        std::string text;
        ::flatbuffers::GenComment(dc, &text, nullptr, prefix);
        code_ += text + "\\";
    }

    // Return a C++ type from the table in idl.h
    std::string GenTypeBasic(const Type& type, bool user_facing_type) const {
        static const char* ctypename[] = {
            // clang-format off
#define FLATBUFFERS_TD(ENUM, IDLTYPE, CTYPE, JTYPE, GTYPE, NTYPE, PTYPE) \
    #CTYPE,
            FLATBUFFERS_GEN_TYPES(FLATBUFFERS_TD)
#undef FLATBUFFERS_TD
            // clang-format on
        };

        if (user_facing_type) {
            if (type.enum_def) {
                return WrapInNameSpace(*type.enum_def);
            }

            if (type.base_type == BASE_TYPE_BOOL) {
                return "bool";
            }
        }

        return ctypename[type.base_type];
    }

    // Return a C++ pointer type, specialized to the actual struct/table types,
    // and vector element types.
    std::string GenTypePointer(const Type& type) const {
        switch (type.base_type) {
        case BASE_TYPE_STRING: {
            return "std::string";
        }

        case BASE_TYPE_VECTOR: {
            const auto type_name = GenTypeWire(type.VectorType(), "", false);
            return "std::vector<" + type_name + ">";
        }

        case BASE_TYPE_STRUCT: {
            return WrapInNameSpace(*type.struct_def);
        }

        case BASE_TYPE_UNION: {
            return GenVariant(type);
        }

        // fall through
        default: {
            return "void";
        }
        }
    }

    std::string GenVariant(const Type& type) const {
        std::string variant;
        for (auto&& temp : type.enum_def->vals.vec) {
            if (temp->union_type.base_type != BASE_TYPE_NONE) {
                if (!variant.empty()) {
                    variant += ", ";
                }
                variant += temp->name;
            }
        }

        return "boost::variant<" + variant + ">";
    }

    // Return a C++ type for any type (scalar/pointer) specifically for
    // building a flatbuffer.
    std::string GenTypeWire(const Type& type, const char* postfix,
                            bool user_facing_type) const {
        if (IsScalar(type.base_type)) {
            return GenTypeBasic(type, user_facing_type) + postfix;
        } else if (IsStruct(type)) {
            return "const " + GenTypePointer(type) + " *";
        } else {
            return "flatbuffers::Offset<" + GenTypePointer(type) + ">" + postfix;
        }
    }

    static std::string NativeName(const std::string& name, const StructDef* sd, const IDLOptions& opts) {
        return sd && !sd->fixed ? opts.object_prefix + name + opts.object_suffix : name;
    }


    const std::string& PtrType(const FieldDef* field) {
        auto attr = field ? field->attributes.Lookup("cpp_ptr_type") : nullptr;
        return attr ? attr->constant : parser_.opts.cpp_object_api_pointer_type;
    }

    const std::string NativeString(const FieldDef* field) {
        auto attr = field ? field->attributes.Lookup("cpp_str_type") : nullptr;
        auto& ret = attr ? attr->constant : parser_.opts.cpp_object_api_string_type;

        if (ret.empty()) {
            return "std::string";
        }

        return ret;
    }

    std::string GenTypeNativePtr(const std::string& type, const FieldDef* field,
                                 bool is_constructor) {
        auto& ptr_type = PtrType(field);

        if (ptr_type != "naked") {
            return ptr_type + "<" + type + ">";
        } else if (is_constructor) {
            return "";
        } else {
            return type + " *";
        }
    }

    std::string GenTypeNative(const Type& type, bool invector,
                              const FieldDef& field) {
        switch (type.base_type) {
        case BASE_TYPE_STRING: {
            return NativeString(&field);
        }

        case BASE_TYPE_VECTOR: {
            const auto type_name = GenTypeNative(type.VectorType(), true, field);

            if (type.struct_def &&
                    type.struct_def->attributes.Lookup("native_custom_alloc")) {
                auto native_custom_alloc =
                    type.struct_def->attributes.Lookup("native_custom_alloc");
                return "std::vector<" + type_name + "," +
                       native_custom_alloc->constant + "<" + type_name + ">>";
            } else {
                return "std::vector<" + type_name + ">";
            }
        }

        case BASE_TYPE_STRUCT: {
            auto type_name = WrapInNameSpace(*type.struct_def);

            if (IsStruct(type)) {
                auto native_type = type.struct_def->attributes.Lookup("native_type");

                if (native_type) {
                    type_name = native_type->constant;
                }

                return type_name;
            } else {
                return NativeName(type_name, type.struct_def, parser_.opts);
            }
        }

        case BASE_TYPE_UNION: {
            return GenVariant(type);
        }

        default: {
            return GenTypeBasic(type, true);
        }
        }
    }

    // Return a C++ type for any type (scalar/pointer) specifically for
    // using a flatbuffer.
    std::string GenTypeGet(const Type& type, const char* afterbasic,
                           const char* beforeptr, const char* afterptr,
                           bool user_facing_type) {
        if (IsScalar(type.base_type)) {
            return GenTypeBasic(type, user_facing_type) + afterbasic;
        } else {
            return beforeptr + GenTypePointer(type) + afterptr;
        }
    }

    std::string GenEnumDecl(const EnumDef& enum_def) const {
        const IDLOptions& opts = parser_.opts;
        return (opts.scoped_enums ? "enum class " : "enum ") + Name(enum_def);
    }

    std::string GenEnumValDecl(const EnumDef& enum_def,
                               const std::string& enum_val) const {
        const IDLOptions& opts = parser_.opts;
        return opts.prefixed_enums ? Name(enum_def) + "_" + enum_val : enum_val;
    }

    std::string GetEnumValUse(const EnumDef& enum_def,
                              const EnumVal& enum_val) const {
        const IDLOptions& opts = parser_.opts;

        if (opts.scoped_enums) {
            return Name(enum_def) + "::" + Name(enum_val);
        } else if (opts.prefixed_enums) {
            return Name(enum_def) + "_" + Name(enum_val);
        } else {
            return Name(enum_val);
        }
    }

    // Generate an enum declaration,
    // an enum string lookup table,
    // and an enum array of values
    void GenEnum(const EnumDef& enum_def) {
        code_.SetValue("ENUM_NAME", Name(enum_def));
        code_.SetValue("BASE_TYPE", GenTypeBasic(enum_def.underlying_type, false));
        code_.SetValue("SEP", "");
        GenComment(enum_def.doc_comment);
        code_ += GenEnumDecl(enum_def) + "\\";

        if (parser_.opts.scoped_enums) {
            code_ += " : {{BASE_TYPE}}\\";
        }

        code_ += " {";
        int64_t anyv = 0;
        const EnumVal* minv = nullptr, *maxv = nullptr;

        for (auto it = enum_def.vals.vec.begin(); it != enum_def.vals.vec.end();
                ++it) {
            const auto& ev = **it;
            GenComment(ev.doc_comment, "  ");
            code_.SetValue("KEY", GenEnumValDecl(enum_def, Name(ev)));
            code_.SetValue("VALUE", NumToString(ev.value));
            code_ += "{{SEP}}  {{KEY}} = {{VALUE}}\\";
            code_.SetValue("SEP", ",\n");
            minv = !minv || minv->value > ev.value ? &ev : minv;
            maxv = !maxv || maxv->value < ev.value ? &ev : maxv;
            anyv |= ev.value;
        }

        if (parser_.opts.scoped_enums || parser_.opts.prefixed_enums) {
            assert(minv && maxv);
            code_.SetValue("SEP", ",\n");

            if (enum_def.attributes.Lookup("bit_flags")) {
                code_.SetValue("KEY", GenEnumValDecl(enum_def, "NONE"));
                code_.SetValue("VALUE", "0");
                code_ += "{{SEP}}  {{KEY}} = {{VALUE}}\\";
                code_.SetValue("KEY", GenEnumValDecl(enum_def, "ANY"));
                code_.SetValue("VALUE", NumToString(anyv));
                code_ += "{{SEP}}  {{KEY}} = {{VALUE}}\\";
            } else {  // MIN & MAX are useless for bit_flags
                code_.SetValue("KEY", GenEnumValDecl(enum_def, "MIN"));
                code_.SetValue("VALUE", GenEnumValDecl(enum_def, minv->name));
                code_ += "{{SEP}}  {{KEY}} = {{VALUE}}\\";
                code_.SetValue("KEY", GenEnumValDecl(enum_def, "MAX"));
                code_.SetValue("VALUE", GenEnumValDecl(enum_def, maxv->name));
                code_ += "{{SEP}}  {{KEY}} = {{VALUE}}\\";
            }
        }

        code_ += "";
        code_ += "};";

        if (parser_.opts.scoped_enums && enum_def.attributes.Lookup("bit_flags")) {
            code_ += "DEFINE_BITMASK_OPERATORS({{ENUM_NAME}}, {{BASE_TYPE}})";
        }

        code_ += "";
        // Generate an array of all enumeration values
        auto num_fields = NumToString(enum_def.vals.vec.size());
        code_ += "inline {{ENUM_NAME}} (&EnumValues{{ENUM_NAME}}())[" + num_fields +
                 "] {";
        code_ += "  static {{ENUM_NAME}} values[] = {";

        for (auto it = enum_def.vals.vec.begin(); it != enum_def.vals.vec.end();
                ++it) {
            const auto& ev = **it;
            auto value = GetEnumValUse(enum_def, ev);
            auto suffix = *it != enum_def.vals.vec.back() ? "," : "";
            code_ += "    " + value + suffix;
        }

        code_ += "  };";
        code_ += "  return values;";
        code_ += "}";
        code_ += "";

        // Generate a generate string table for enum values.
        // Problem is, if values are very sparse that could generate really big
        // tables. Ideally in that case we generate a map lookup instead, but for
        // the moment we simply don't output a table at all.
        auto range =
            enum_def.vals.vec.back()->value - enum_def.vals.vec.front()->value + 1;
        // Average distance between values above which we consider a table
        // "too sparse". Change at will.
        static const int kMaxSparseness = 5;

        if (range / static_cast<int64_t>(enum_def.vals.vec.size()) <
                kMaxSparseness) {
            code_ += "inline const char **EnumNames{{ENUM_NAME}}() {";
            code_ += "  static const char *names[] = {";
            auto val = enum_def.vals.vec.front()->value;

            for (auto it = enum_def.vals.vec.begin(); it != enum_def.vals.vec.end();
                    ++it) {
                const auto& ev = **it;

                while (val++ != ev.value) {
                    code_ += "    \"\",";
                }

                code_ += "    \"" + Name(ev) + "\",";
            }

            code_ += "    nullptr";
            code_ += "  };";
            code_ += "  return names;";
            code_ += "}";
            code_ += "";
            code_ += "inline const char *EnumName{{ENUM_NAME}}({{ENUM_NAME}} e) {";
            code_ += "  const size_t index = static_cast<int>(e)\\";

            if (enum_def.vals.vec.front()->value) {
                auto vals = GetEnumValUse(enum_def, *enum_def.vals.vec.front());
                code_ += " - static_cast<int>(" + vals + ")\\";
            }

            code_ += ";";
            code_ += "  return EnumNames{{ENUM_NAME}}()[index];";
            code_ += "}";
            code_ += "";
        }
    }

    // Generates a value with optionally a cast applied if the field has a
    // different underlying type from its interface type (currently only the
    // case for enums. "from" specify the direction, true meaning from the
    // underlying type to the interface type.
    std::string GenUnderlyingCast(const FieldDef& field, bool from,
                                  const std::string& val) {
        if (from && field.value.type.base_type == BASE_TYPE_BOOL) {
            return val + " != 0";
        } else if ((field.value.type.enum_def &&
                    IsScalar(field.value.type.base_type)) ||
                   field.value.type.base_type == BASE_TYPE_BOOL) {
            return "static_cast<" + GenTypeBasic(field.value.type, from) + ">(" +
                   val + ")";
        } else {
            return val;
        }
    }

    void GenFullyQualifiedNameGetter(const StructDef& struct_def,
                                     const std::string& name) {
        if (!parser_.opts.generate_name_strings) {
            return;
        }

        auto fullname = struct_def.defined_namespace->GetFullyQualifiedName(name);
        code_.SetValue("NAME", fullname);
        code_.SetValue("CONSTEXPR", "FLATBUFFERS_CONSTEXPR");
        code_ += "  static {{CONSTEXPR}} const char *GetFullyQualifiedName() {";
        code_ += "    return \"{{NAME}}\";";
        code_ += "  }";
    }

    std::string GenDefaultConstant(const FieldDef& field) {
        return field.value.type.base_type == BASE_TYPE_FLOAT
               ? field.value.constant + "f"
               : field.value.constant;
    }

    std::string GetDefaultScalarValue(const FieldDef& field) {
        if (field.value.type.enum_def && IsScalar(field.value.type.base_type)) {
            auto ev = field.value.type.enum_def->ReverseLookup(
                          StringToInt(field.value.constant.c_str()), false);

            if (ev) {
                return WrapInNameSpace(field.value.type.enum_def->defined_namespace,
                                       GetEnumValUse(*field.value.type.enum_def, *ev));
            } else {
                return GenUnderlyingCast(field, true, field.value.constant);
            }
        } else if (field.value.type.base_type == BASE_TYPE_BOOL) {
            return field.value.constant == "0" ? "false" : "true";
        } else {
            return GenDefaultConstant(field);
        }
    }

    // Generate a member, including a default value for scalars and raw pointers.
    void GenMember(const FieldDef& field) {
        if (!field.deprecated) {
            auto type = GenTypeNative(field.value.type, false, field);
            if (!field.required) {
                type = "boost::optional<" + type + ">";
            }
            auto cpp_type = field.attributes.Lookup("cpp_type");
            auto full_type = (cpp_type ? cpp_type->constant + " *" : type + " ");
            auto comment = (field.required ? " // required" : "");
            code_.SetValue("FIELD_TYPE", full_type);
            code_.SetValue("FIELD_NAME", Name(field));
            code_.SetValue("COMMENT", comment);
            code_ += "  {{FIELD_TYPE}}{{FIELD_NAME}};{{COMMENT}}";

        }
    }

    // Generate the default constructor for this struct. Properly initialize all
    // scalar members with default values.
    void GenDefaultConstructor(const StructDef& struct_def) {
        std::string initializer_list;

        for (auto it = struct_def.fields.vec.begin();
                it != struct_def.fields.vec.end(); ++it) {
            const auto& field = **it;

            if (!field.deprecated &&  // Deprecated fields won't be accessible.
                    field.value.type.base_type != BASE_TYPE_UTYPE) {
                auto cpp_type = field.attributes.Lookup("cpp_type");

                // Scalar types get parsed defaults, raw pointers get nullptrs.
                if (IsScalar(field.value.type.base_type)) {
                    if (!initializer_list.empty()) {
                        initializer_list += ",\n        ";
                    }

                    initializer_list += Name(field);
                    initializer_list += "(" + GetDefaultScalarValue(field) + ")";
                } else if (field.value.type.base_type == BASE_TYPE_STRUCT) {
                    if (IsStruct(field.value.type)) {
                        auto native_default = field.attributes.Lookup("native_default");

                        if (native_default) {
                            if (!initializer_list.empty()) {
                                initializer_list += ",\n        ";
                            }

                            initializer_list +=
                                Name(field) + "(" + native_default->constant + ")";
                        }
                    }
                } else if (cpp_type) {
                    if (!initializer_list.empty()) {
                        initializer_list += ",\n        ";
                    }

                    initializer_list += Name(field) + "(0)";
                }
            }
        }

        if (!initializer_list.empty()) {
            initializer_list = "\n      : " + initializer_list;
        }

        code_.SetValue("NATIVE_NAME",
                       NativeName(Name(struct_def), &struct_def, parser_.opts));
        code_.SetValue("INIT_LIST", initializer_list);
        code_ += "  {{NATIVE_NAME}}(){{INIT_LIST}} {";
        code_ += "  }";
    }

    void GenNativeTable(const StructDef& struct_def) {
        const auto native_name = NativeName(Name(struct_def), &struct_def, parser_.opts);
        code_.SetValue("STRUCT_NAME", Name(struct_def));
        code_.SetValue("NATIVE_NAME", native_name);
        code_ += "struct {{NATIVE_NAME}} {";
        for (auto it = struct_def.fields.vec.begin(); it != struct_def.fields.vec.end(); ++it) {
            const auto& field = **it;
            GenMember(field);
        }
        if (!struct_def.fields.vec.empty()) {
            code_ += "";
        }

        GenDefaultConstructor(struct_def);
        code_ += "";

        GenInitConstructor(struct_def);
        GenPersist(struct_def);

        code_ += "};";
        code_ += "";
    }

    void GenInitConstructor(const StructDef& struct_def) {
        std::string initializer_list;
        std::string required_member_list;

        for (auto it = struct_def.fields.vec.begin(); it != struct_def.fields.vec.end(); ++it) {
            const auto& field = **it;
            if (!field.deprecated && field.value.type.base_type != BASE_TYPE_UTYPE) {
                auto cpp_type = field.attributes.Lookup("cpp_type");

                // Scalar types get parsed defaults, raw pointers get nullptrs.
                if (IsScalar(field.value.type.base_type)) {
                    if (!initializer_list.empty()) {
                        initializer_list += ",\n        ";
                    }

                    initializer_list += Name(field);
                    initializer_list += "(" + GetDefaultScalarValue(field) + ")";
                } else if (field.value.type.base_type == BASE_TYPE_STRUCT) {
                    if (IsStruct(field.value.type)) {
                        auto native_default = field.attributes.Lookup("native_default");

                        if (native_default) {
                            if (!initializer_list.empty()) {
                                initializer_list += ",\n        ";
                            }

                            initializer_list +=
                                Name(field) + "(" + native_default->constant + ")";
                        }
                    }
                } else if (cpp_type) {
                    if (!initializer_list.empty()) {
                        initializer_list += ",\n        ";
                    }

                    initializer_list += Name(field) + "(0)";
                }
            }

            if (!field.deprecated && field.required) {
                if (!required_member_list.empty()) {
                    required_member_list += ", ";
                }

                if (!initializer_list.empty()) {
                    initializer_list += ",\n        ";
                }

                const auto member_name = Name(field);
                const auto arg_name = "_" + Name(field);
                const auto arg_type = GenTypeGet(field.value.type, " ", "const ", "& ", true);

                required_member_list += arg_type + arg_name;
                initializer_list += member_name + "(" + arg_name + ")";
            }
        }

        if (!initializer_list.empty()) {
            initializer_list = "\n      : " + initializer_list;
        }

        if (!required_member_list.empty()) {
            code_.SetValue("NATIVE_NAME", NativeName(Name(struct_def), &struct_def, parser_.opts));
            code_.SetValue("INIT_LIST", initializer_list);
            code_.SetValue("REQUIRED_MEMBER_LIST", required_member_list);
            code_ += "  {{NATIVE_NAME}}({{REQUIRED_MEMBER_LIST}}){{INIT_LIST}} {";
            code_ += "  }";
            code_ += "";
        }
    }

    void GenPersist(const StructDef& struct_def) {
        std::string member_list;
        code_ += "  template <typename Archive>";
        code_ += "  void Persist(Archive& ar) {";
        for (auto it = struct_def.fields.vec.begin(); it != struct_def.fields.vec.end(); ++it) {
            const auto& field = **it;
            if (!field.deprecated) {
                const auto member_name = Name(field);
                member_list += "& MEMBER_VARIABLE(" + member_name + ")\n         ";
            }
        }
        code_.SetValue("MEMBER_LIST", member_list);
        code_ += "      ar {{MEMBER_LIST}};";
        code_ += "  }";
    }

    // Generate an accessor struct, builder structs & function for a table.
    void GenTable(const StructDef& struct_def) {
        GenNativeTable(struct_def);
    }

    static void GenPadding(
        const FieldDef& field, std::string* code_ptr, int* id,
        const std::function<void(int bits, std::string* code_ptr, int* id)>& f) {
        if (field.padding) {
            for (int i = 0; i < 4; i++) {
                if (static_cast<int>(field.padding) & (1 << i)) {
                    f((1 << i) * 8, code_ptr, id);
                }
            }

            assert(!(field.padding & ~0xF));
        }
    }

    static void PaddingDefinition(int bits, std::string* code_ptr, int* id) {
        *code_ptr += "  int" + NumToString(bits) + "_t padding" +
                     NumToString((*id)++) + "__;";
    }

    static void PaddingInitializer(int bits, std::string* code_ptr, int* id) {
        (void)bits;
        *code_ptr += ",\n        padding" + NumToString((*id)++) + "__(0)";
    }

    static void PaddingNoop(int bits, std::string* code_ptr, int* id) {
        (void)bits;
        *code_ptr += "    (void)padding" + NumToString((*id)++) + "__;";
    }

    // Generate an accessor struct with constructor for a flatbuffers struct.
    void GenStruct(const StructDef& struct_def) {
        GenComment(struct_def.doc_comment);
        code_.SetValue("ALIGN", NumToString(struct_def.minalign));
        code_.SetValue("STRUCT_NAME", Name(struct_def));
        code_ += "struct {{STRUCT_NAME}} {";

        // generate fields
        for (auto it = struct_def.fields.vec.begin(); it != struct_def.fields.vec.end(); ++it) {
            const auto& field = **it;
            code_.SetValue("FIELD_TYPE", GenTypeGet(field.value.type, " ", "", " ", false));
            code_.SetValue("FIELD_NAME", Name(field));
            code_ += "  {{FIELD_TYPE}}{{FIELD_NAME}};";
        }
        code_ += "";

        // generate constructors
        code_ += "  {{STRUCT_NAME}}() {";
        code_ += "    memset(this, 0, sizeof({{STRUCT_NAME}}));";
        code_ += "  }";
        code_ += "";

        std::string arg_list;
        std::string init_list;
        int padding_id = 0;
        for (auto it = struct_def.fields.vec.begin(); it != struct_def.fields.vec.end(); ++it) {
            const auto& field = **it;
            const auto member_name = Name(field);
            const auto arg_name = "_" + Name(field);
            const auto arg_type = GenTypeGet(field.value.type, " ", "const ", " &", true);

            if (it != struct_def.fields.vec.begin()) {
                arg_list += ", ";
                init_list += ",\n        ";
            }

            arg_list += arg_type;
            arg_list += arg_name;
            init_list += member_name;

            if (IsScalar(field.value.type.base_type)) {
                auto type = GenUnderlyingCast(field, false, arg_name);
                init_list += "(" + type + ")";
            } else {
                init_list += "(" + arg_name + ")";
            }

            if (field.padding) {
                GenPadding(field, &init_list, &padding_id, PaddingInitializer);
            }
        }

        code_.SetValue("ARG_LIST", arg_list);
        code_.SetValue("INIT_LIST", init_list);
        code_ += "  {{STRUCT_NAME}}({{ARG_LIST}})";
        code_ += "      : {{INIT_LIST}} {";
        padding_id = 0;

        for (auto it = struct_def.fields.vec.begin();
                it != struct_def.fields.vec.end(); ++it) {
            const auto& field = **it;

            if (field.padding) {
                std::string padding;
                GenPadding(field, &padding, &padding_id, PaddingNoop);
                code_ += padding;
            }
        }

        code_ += "  }";
        code_ += "";

        // generate persist method
        GenPersist(struct_def);

        code_ += "};";
        code_ += "";
    }

    // Set up the correct namespace. Only open a namespace if the existing one is
    // different (closing/opening only what is necessary).
    //
    // The file must start and end with an empty (or null) namespace so that
    // namespaces are properly opened and closed.
    void SetNameSpace(const Namespace* ns) {
        if (cur_name_space_ == ns) {
            return;
        }

        // Compute the size of the longest common namespace prefix.
        // If cur_name_space is A::B::C::D and ns is A::B::E::F::G,
        // the common prefix is A::B:: and we have old_size = 4, new_size = 5
        // and common_prefix_size = 2
        size_t old_size = cur_name_space_ ? cur_name_space_->components.size() : 0;
        size_t new_size = ns ? ns->components.size() : 0;
        size_t common_prefix_size = 0;

        while (common_prefix_size < old_size && common_prefix_size < new_size &&
                ns->components[common_prefix_size] ==
                cur_name_space_->components[common_prefix_size]) {
            common_prefix_size++;
        }

        // Close cur_name_space in reverse order to reach the common prefix.
        // In the previous example, D then C are closed.
        for (size_t j = old_size; j > common_prefix_size; --j) {
            code_ += "}  // namespace " + cur_name_space_->components[j - 1];
        }

        if (old_size != common_prefix_size) {
            code_ += "";
        }

        // open namespace parts to reach the ns namespace
        // in the previous example, E, then F, then G are opened
        for (auto j = common_prefix_size; j != new_size; ++j) {
            code_ += "namespace " + ns->components[j] + " {";
        }

        if (new_size != common_prefix_size) {
            code_ += "";
        }

        cur_name_space_ = ns;
    }
};

}  // namespace cpp

bool GenerateLmiCPP(const Parser& parser, const std::string& path,
                 const std::string& file_name) {
    cpp::LmiCppGenerator generator(parser, path, file_name);
    return generator.generate();
}

std::string LmiCPPMakeRule(const Parser& parser, const std::string& path,
                           const std::string& file_name) {
    const auto filebase = flatbuffers::StripPath(flatbuffers::StripExtension(file_name));
    const auto included_files = parser.GetIncludedFilesRecursive(file_name);
    std::string make_rule = GeneratedFileName(path, filebase) + ": ";

    for (auto it = included_files.begin(); it != included_files.end(); ++it) {
        make_rule += " " + *it;
    }

    return make_rule;
}

}  // namespace flatbuffers
