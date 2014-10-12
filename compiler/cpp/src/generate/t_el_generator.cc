/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sstream>
#include <algorithm>
#include "t_generator.h"
#include "platform.h"
#include "version.h"

using std::map;
using std::ofstream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

static const string endl = "\n";  // avoid ostream << std::endl flushes

/**
 * Elisp code generator.
 *
 */
class t_el_generator : public t_generator {
 public:
  t_el_generator(
      t_program* program,
      const std::map<std::string, std::string>& parsed_options,
      const std::string& option_string)
    : t_generator(program)
  {
    (void) option_string;
    std::map<std::string, std::string>::const_iterator iter;

    iter = parsed_options.find("utf8strings");
    gen_utf8strings_ = (iter != parsed_options.end());

    copy_options_ = option_string;

    out_dir_base_ = "gen-el";
  }

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */

  void generate_typedef  (t_typedef*  ttypedef);
  void generate_enum     (t_enum*     tenum);
  void generate_const    (t_const*    tconst);
  void generate_struct   (t_struct*   tstruct);
  void generate_xception (t_struct*   txception);
  void generate_service  (t_service*  tservice);

  std::string render_const_value(t_type* type, t_const_value* value);

  /**
   * Struct generation code
   */

  void generate_el_struct(t_struct* tstruct, bool is_exception);
  void generate_el_struct_definition(std::ofstream& out, t_struct* tstruct, bool is_xception=false, bool is_result=false);
  void generate_el_struct_reader(std::ofstream& out, t_struct* tstruct, bool is_exception, bool is_result);
  void generate_el_struct_writer(std::ofstream& out, t_struct* tstruct, bool is_exception, bool is_result);
  void generate_el_struct_required_validator(std::ofstream& out, t_struct* tstruct);
  //void generate_el_function_helpers(t_function* tfunction);

  /**
   * Service-level generation functions
   */

  void generate_service_helpers   (t_service*  tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_remote    (t_service* tservice);
  void generate_service_server    (t_service* tservice);
  void generate_process_function  (t_service* tservice, t_function* tfunction);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (std::ofstream &out,
					  t_field*    tfield,
					  std::string prefix="",
					  bool inclass=false);

  void generate_deserialize_struct       (std::ofstream &out,
					  t_struct*   tstruct,
					  std::string prefix="");

  void generate_deserialize_container    (std::ofstream &out,
					  t_type*     ttype,
					  std::string prefix="");

  void generate_deserialize_set_element  (std::ofstream &out,
					  t_set*      tset,
					  std::string prefix="");

  void generate_deserialize_map_element  (std::ofstream &out,
					  t_map*      tmap,
					  std::string prefix="");

  void generate_deserialize_list_element (std::ofstream &out,
					  t_list*     tlist,
					  std::string prefix="");

  void generate_serialize_field          (std::ofstream &out,
					  t_field*    tfield,
					  std::string prefix="");

  void generate_serialize_struct         (std::ofstream &out,
					  t_struct*   tstruct,
					  std::string prefix="");

  void generate_serialize_container      (std::ofstream &out,
					  t_type*     ttype,
					  std::string prefix="");

  void generate_serialize_map_element    (std::ofstream &out,
					  t_map*      tmap,
					  std::string kiter,
					  std::string viter);

  void generate_serialize_set_element    (std::ofstream &out,
					  t_set*      tmap,
					  std::string iter);

  void generate_serialize_list_element   (std::ofstream &out,
					  t_list*     tlist,
					  std::string iter);

  void generate_elisp_docstring         (std::ofstream& out,
					  t_struct* tstruct);

  void generate_elisp_docstring         (std::ofstream& out,
					  t_function* tfunction);

  void generate_elisp_docstring         (std::ofstream& out,
					  t_doc*    tdoc,
					  t_struct* tstruct,
					  const char* subheader);

  void generate_elisp_docstring         (std::ofstream& out,
					  t_doc* tdoc);

  /**
   * Helper rendering functions
   */

  std::string el_autogen_comment();
  std::string el_imports();
  std::string render_includes();
  std::string declare_argument(t_field* tfield);
  std::string render_field_default_value(t_field* tfield);
  std::string type_name(t_type* ttype);
  std::string function_signature(t_function* tfunction);
  std::string argument_list(t_struct* tstruct,
			    std::vector<std::string> *pre=NULL,
			    std::vector<std::string> *post=NULL);
  std::string type_to_enum(t_type* ttype);
  std::string type_to_spec_args(t_type* ttype);

  static bool is_valid_namespace(const std::string& sub_namespace) {
    return sub_namespace == "twisted";
  }

  static std::string get_real_el_module(const t_program* program) {

    std::string real_module = program->get_namespace("el");
    if (real_module.empty()) {
      return program->get_name();
    }
    return real_module;
  }

 private:

  std::string copy_options_;

  /**
   * True if strings should be encoded using utf-8.
   */
  bool gen_utf8strings_;

  /**
   * File streams
   */
  std::ofstream f_types_;
  std::ofstream f_consts_;
  std::ofstream f_service_;

  std::string package_dir_;
  std::string module_;

};


/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_el_generator::init_generator()
{
  // Make output directories
  string module = get_real_el_module(program_);
  package_dir_ = get_out_dir();
  module_ = module;
  while (true) {
    // TODO: Do better error checking here.
    MKDIR(package_dir_.c_str());
    if (module.empty()) {
      break;
    }
    string::size_type pos = module.find('.');
    if (pos == string::npos) {
      package_dir_ += "/";
      package_dir_ += module;
      module.clear();
    } else {
      package_dir_ += "/";
      package_dir_ += module.substr(0, pos);
      module.erase(0, pos+1);
    }
  }

  // Make output files
  string f_types_name = package_dir_+"/"+"thrift-gen-"+module_+"-types.el";
  f_types_.open(f_types_name.c_str());

  string f_consts_name = package_dir_+"/"+"thrift-gen-"+module_+"-constants.el";
  f_consts_.open(f_consts_name.c_str());

  // Print header
  f_types_ <<
    el_autogen_comment() << endl <<
    el_imports() << endl <<
    render_includes() << endl << endl;

  f_consts_ <<
    el_autogen_comment() << endl <<
    el_imports() << endl <<
    "(require 'thrift-gen-" << module_ << "-types)" << endl << endl;
}

/**
 * Renders all the imports necessary for including another Thrift program
 */
string t_el_generator::render_includes() {
  const vector<t_program*>& includes = program_->get_includes();
  string result = "";
  for (size_t i = 0; i < includes.size(); ++i) {
    result += "(require 'thrift-gen-" + get_real_el_module(includes[i]) + "-types)\n";
  }
  if (includes.size() > 0) {
    result += "\n";
  }
  return result;
}

/**
 * Autogen'd comment
 */
string t_el_generator::el_autogen_comment() {
  return
    std::string(";;\n") +
    ";; Autogenerated by Thrift Compiler (" + THRIFT_VERSION + ")\n" +
    ";;\n" +
    ";; DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n" +
    ";;\n" +
    ";;  options string: " + copy_options_  + "\n" +
    ";;\n";
}

/**
 * Prints standard thrift imports
 */
string t_el_generator::el_imports() {
  return
    string("(require 'thrift)");
}

/**
 * Closes the type files
 */
void t_el_generator::close_generator() {
  // Print trailer
  f_types_ <<
    "(provide 'thrift-gen-" << module_ << "-types)" << endl;
  f_consts_ <<
    "(provide 'thrift-gen-" << module_ << "-constants)" << endl;

  // Close types file
  f_types_.close();
  f_consts_.close();
}

/**
 * Generates a typedef. This is not done in Elisp, types are all implicit.
 *
 * @param ttypedef The type definition
 */
void t_el_generator::generate_typedef(t_typedef* ttypedef) {
  (void) ttypedef;
}

/**
 * Generates code for an enumerated type. Done using a class to scope
 * the values.
 *
 * @param tenum The enumeration
 */
void t_el_generator::generate_enum(t_enum* tenum) {
  std::ostringstream to_string_mapping, from_string_mapping;

  string module = get_real_el_module(tenum->get_program());

  generate_elisp_docstring(f_types_, tenum);
  f_types_ << endl;

  // Function to convert enum symbols to int
  f_types_ <<
    "(defun thrift-gen-" << module << "-" << tenum->get_name() << "-to-int (in)" << endl;
  string beg_cond = "(cond ";
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();
    f_types_ <<
      "  "  << beg_cond << "((eq in '" <<
      escape_string((*c_iter)->get_name()) << ") " << value << ")";
    beg_cond = "\n        ";
  }
  f_types_ << "\n" <<
    "        " << "((symbolp in)\n" <<
    "        " << " (error \"%s is not a valid value of the enum '" << tenum->get_name() << "'.\"\n" <<
    "        " << "        (symbol-name in))\n" <<
    "        " << "(t\n" <<
    "        " << " (error \"Wrong value used for enum '" << tenum->get_name() << "'.\")))))\n" << endl;


  // Function to convert enum symbols from int
  f_types_<<
    "(defun thrift-gen-" << module << "-int-to-" << tenum->get_name() << " (in)" << endl;
  beg_cond = "(cond ";
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();
    f_types_ <<
      "  " << beg_cond << "((eq in " <<
      value << ") '" << escape_string((*c_iter)->get_name()) << ")";
    beg_cond = "\n        ";
  }
  f_types_ << "\n" <<
    "        " << "((integerp in)\n" <<
    "        " << " (error \"%s is not a valid value of the enum '" << tenum->get_name() << "'.\"))\n" <<
    "        " << "(t\n" <<
    "        " << " (error \"Wrong value used for enum '" << tenum->get_name() << "'.\"))))\n\n" << endl;

  // Function to read an enum
  f_types_ <<
    "(defun thrift-gen-tutorial-read-" << tenum->get_name() << " (protocol)\n" <<
    "  \"Read '" << tenum->get_name() << "' enum.\"\n" <<
    "  (thrift-gen-tutorial-int-to-" << tenum->get_name() << " (thrift-protocol-read-i32 protocol)))\n\n";

  f_types_ <<
    "(defun thrift-gen-tutorial-write-" << tenum->get_name() << " (protocol in)\n" <<
    "  \"Write '" << tenum->get_name() << "' enum.\"\n" <<
    "  (thrift-protocol-write-i32 protocol\n"
    "                             (thrift-gen-tutorial-" << tenum->get_name() << "-to-int in)))\n\n" << endl;
}

/**
 * Generate a constant value
 */
void t_el_generator::generate_const(t_const* tconst) {
  t_type* type = tconst->get_type();
  string name = tconst->get_name();
  t_const_value* value = tconst->get_value();

  indent(f_consts_) << "(setq " << name << " " << render_const_value(type, value) << ")\n";
  f_consts_ << endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
string t_el_generator::render_const_value(t_type* type, t_const_value* value) {
  type = get_true_type(type);
  std::ostringstream out;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      out << '"' << get_escaped_string(value) << '"';
      break;
    case t_base_type::TYPE_BOOL:
      out << (value->get_integer() > 0 ? "t" : "nil");
      break;
    case t_base_type::TYPE_BYTE:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      out << value->get_integer();
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
	out << value->get_integer();
      } else {
	out << value->get_double();
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    indent(out) << value->get_integer();
  } else if (type->is_struct() || type->is_xception()) {
    out << "'(" << endl;
    indent_up();
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      t_type* field_type = NULL;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
	if ((*f_iter)->get_name() == v_iter->first->get_string()) {
	  field_type = (*f_iter)->get_type();
	}
      }
      if (field_type == NULL) {
	throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }
      out << indent();
      out << ":" << render_const_value(g_type_string, v_iter->first);
      out << " " << render_const_value(field_type, v_iter->second);
      out << " " << endl;
    }
    indent_down();
    indent(out) << ")";
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    out << "'(";
    indent_up();
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      out << "\n" << indent();
      out << render_const_value(ktype, v_iter->first);
      out << " ";
      out << render_const_value(vtype, v_iter->second);
      out << "";
    }
    indent_down();
    indent(out) << ")";
  } else if (type->is_list() || type->is_set()) {
    t_type* etype;
    if (type->is_list()) {
      etype = ((t_list*)type)->get_elem_type();
    } else {
      etype = ((t_set*)type)->get_elem_type();
    }
    if (type->is_set()) {
      out << "'(";
    }
    indent_up();
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      out << indent();
      out << render_const_value(etype, *v_iter);
      out << " " << endl;
    }
    indent_down();
    if (type->is_set()) {
      out << ")";
    }
  } else {
    throw "CANNOT GENERATE CONSTANT FOR TYPE: " + type->get_name();
  }

  return out.str();
}

/**
 * Generates a Elisp struct
 */
void t_el_generator::generate_struct(t_struct* tstruct) {
  generate_el_struct(tstruct, false);
}

/**
 * Generates a struct definition for a thrift exception. Basically the same
 * as a struct but extends the Exception class.
 *
 * @param txception The struct definition
 */
void t_el_generator::generate_xception(t_struct* txception) {
  generate_el_struct(txception, true);
}

/**
 * Generates a Elisp struct
 */
void t_el_generator::generate_el_struct(t_struct* tstruct,
					bool is_exception) {
  generate_el_struct_definition(f_types_, tstruct, is_exception);
}

/**
 * Generates a struct definition for a thrift data type.
 *
 * @param tstruct The struct definition
 */
void t_el_generator::generate_el_struct_definition(ofstream& out,
						   t_struct* tstruct,
						   bool is_exception,
						   bool is_result) {
  generate_elisp_docstring(out, tstruct);

  (void) is_result;
  generate_el_struct_reader(out, tstruct, is_exception, is_result);
  generate_el_struct_writer(out, tstruct, is_exception, is_result);
}

/**
 * Generates the read method for a struct
 */
void t_el_generator::generate_el_struct_reader(ofstream& out,
					       t_struct* tstruct,
					       bool is_exception,
					       bool is_result) {

  const vector<t_field*>& sorted_members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator m_iter;
  string structname = tstruct->get_name();

  out << std::endl <<
    "(defun thrift-gen-" << get_real_el_module(tstruct->get_program()) << "-read-" << structname << " (protocol)"<< endl;
  indent_up();

  out << indent() << "\"Read " << structname;
  if(is_exception) {
    out << " exception.\"";
  } else {
    out << " struct.\"";
  }
  out << endl;

  string beg = "(let (";
  for (m_iter = sorted_members.begin(); m_iter != sorted_members.end(); ++m_iter) {
    string field_name = (*m_iter)->get_name();
    out << indent() <<
      beg << "(res-" << field_name << " nil)";
    beg = "\n        ";
  }
  out << ")" << endl;

  out <<
    indent() << "  (thrift-protocol-read-struct-begin protocol)\n" <<
    indent() << "  (catch 'break\n" <<
    indent() << "    (while t\n" <<
    indent() << "      (let* ((r (thrift-protocol-read-field-begin protocol))\n" <<
    indent() << "             (fname (pop r))\n" <<
    indent() << "             (ftype (pop r))\n" <<
    indent() << "             (fid (pop r)))\n" <<
    indent() << "        (cond ((equal ftype (thrift-constant-type 'stop))\n" <<
    indent() << "               (throw 'break t))\n";

  for (m_iter = sorted_members.begin(); m_iter != sorted_members.end(); ++m_iter) {
    string field_name = (*m_iter)->get_name();
    string field_type = (*m_iter)->get_type()->get_name();
    int32_t field_key = (*m_iter)->get_key();
    string field_type_type = "'i32";
    if ((*m_iter)->get_type()->is_base_type()) {
      field_type_type = (*m_iter)->get_type()->get_name();
    } else if ((*m_iter)->get_type()->is_struct()) {
      field_type_type = "struct";
    } else if ((*m_iter)->get_type()->is_map()) {
      field_type_type = "map";
    } else if ((*m_iter)->get_type()->is_set()) {
      field_type_type = "set";
    } else if ((*m_iter)->get_type()->is_enum()) {
      field_type_type = "i32";
    }

    out <<
      indent() << "              ((equal fid " << field_key << ") ; '" << field_name << "' element\n" <<
      indent() << "               (if (equal ftype (thrift-constant-type '" << field_type_type << "))\n";

    if ((*m_iter)->get_type()->is_struct() || (*m_iter)->get_type()->is_enum()) {
      out <<
	indent() << "                   (setq res-" << field_name << " (thrift-gen-" <<
	get_real_el_module((*m_iter)->get_type()->get_program()) << "-read-" <<
	(*m_iter)->get_type()->get_name() << " protocol))\n";
    } else {
      out <<
	indent() << "                   (setq res-" << field_name <<
	" (thrift-protocol-read-" << field_type_type << " protocol))\n";
    }

    out <<
      indent() << "                 (thrift-protocol-skip protocol ftype)))\n";
  }

  out <<
    indent() << "              (t\n" <<
    indent() << "               (thrift-protocol-skip protocol ftype)))\n";

  out <<
    indent() << "        (thrift-protocol-read-field-end protocol))))\n" <<
    indent() << "  (thrift-protocol-read-struct-end protocol)\n";

  out <<
    indent() << "  (list";
  for (m_iter = sorted_members.begin(); m_iter != sorted_members.end(); ++m_iter) {
    string field_name = (*m_iter)->get_name();
    out << " res-" << field_name;
  }
  out << ")))\n" << endl;

  indent_down();

}


void t_el_generator::generate_el_struct_writer(ofstream& out,
					       t_struct* tstruct,
					       bool is_exception,
					       bool is_result) {
  const vector<t_field*>& sorted_members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator m_iter;
  string structname = tstruct->get_name();

  out << std::endl <<
    "(defun thrift-gen-" << get_real_el_module(tstruct->get_program()) << "-write-" << structname << " (protocol in)"<< endl;
  indent_up();

  out << indent() << "\"Write " << structname;
  if(is_exception) {
    out << " exception.\"";
  } else {
    out << " struct.\"";
  }
  out << endl;

  out << indent() << "(thrift-protocol-write-struct-begin protocol \"" << structname << "\")" << endl;

  for (m_iter = sorted_members.begin(); m_iter != sorted_members.end(); ++m_iter) {
    string field_name = (*m_iter)->get_name();
    string field_type = (*m_iter)->get_type()->get_name();
    int32_t field_key = (*m_iter)->get_key();
    string field_type_type = "'i32";
    if ((*m_iter)->get_type()->is_base_type()) {
      field_type_type = (*m_iter)->get_type()->get_name();
    } else if ((*m_iter)->get_type()->is_struct()) {
      field_type_type = "struct";
    } else if ((*m_iter)->get_type()->is_map()) {
      field_type_type = "map";
    } else if ((*m_iter)->get_type()->is_set()) {
      field_type_type = "set";
    } else if ((*m_iter)->get_type()->is_enum()) {
      field_type_type = "i32";
    }

    out <<
      "  ;; Encode " <<  field_name << "\n" <<
      "  (when (plist-get in :" << field_name << ")\n" <<
      "    (thrift-protocol-write-field-begin protocol\n" <<
      "                                       \"" << field_name << "\"\n" <<
      "                                       (thrift-constant-type '" << field_type_type << ")\n" <<
      "                                       " << field_key << ")\n";

    if ((*m_iter)->get_type()->is_struct() || (*m_iter)->get_type()->is_enum()) {
      out <<
	"    (thrift-gen-" << get_real_el_module((*m_iter)->get_type()->get_program()) <<
	"-write-" << (*m_iter)->get_type()->get_name() << " protocol (plist-get in :" << field_name << "))\n";
    } else {
      out <<
	"    (thrift-protocol-write-" << field_type << " protocol (plist-get in :" << field_name << "))\n";
    }

    out <<
      "    (thrift-protocol-write-field-end protocol))" << endl;
  }
  out <<
    "  (thrift-protocol-write-field-stop protocol)\n" <<
    "  (thrift-protocol-write-struct-end protocol))\n\n" << endl;

  indent_down();
}

void t_el_generator::generate_el_struct_required_validator(ofstream& out,
							   t_struct* tstruct) {
  // indent(out) << "def validate(self):" << endl;
  // indent_up();

  // const vector<t_field*>& fields = tstruct->get_members();

  // if (fields.size() > 0) {
  //   vector<t_field*>::const_iterator f_iter;

  //   for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
  //     t_field* field = (*f_iter);
  //     if (field->get_req() == t_field::T_REQUIRED) {
  //	indent(out) << "if self." << field->get_name() << " is None:" << endl;
  //	indent(out) << "  raise TProtocol.TProtocolException(message='Required field " <<
  //	  field->get_name() << " is unset!')" << endl;
  //     }
  //   }
  // }

  // indent(out) << "return" << endl << endl;
  // indent_down();
}

/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_el_generator::generate_service(t_service* tservice) {
  string f_service_name = package_dir_+"/thrift-gen-"+module_+"-"+service_name_+".el";
  f_service_.open(f_service_name.c_str());

  f_service_ <<
    el_autogen_comment() << endl <<
    el_imports() << endl;

  f_service_ <<
    "(require 'thrift-gen-" + module_ + "-types)" << endl <<
    "(require 'thrift-gen-" + module_ + "-constants)" << endl;

  if (tservice->get_extends() != NULL) {
    f_service_ <<
      "(require 'thrift-gen-" << get_real_el_module(tservice->get_extends()->get_program()) <<
      "-" << tservice->get_extends()->get_name() << ")" << endl;
  } else {
    f_service_ <<
      "(require 'thrift-service)" << endl;
  }

  f_service_ << endl;

  // Generate the three main parts of the service
  generate_service_client(tservice);
  generate_service_server(tservice);
  generate_service_helpers(tservice);
  generate_service_remote(tservice);

  f_service_ <<
    "(provide 'thrift-gen-" << module_ << "-" << service_name_ << ")" << endl;

  // Close service file
  f_service_.close();
}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_el_generator::generate_service_helpers(t_service* tservice) {
  string svc_name =  get_real_el_module(tservice->get_program()) + "-" + tservice->get_name();

  f_service_ <<
    indent() << ";; HELPER FUNCTIONS AND STRUCTURES\n\n" << endl;

  // Encoders/decoders definitions
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {

    string fun_name = (*f_iter)->get_name();

    //
    // generate 'write' helper function
    //
    f_service_ <<
      indent() << "(defun thrift-gen-" << svc_name << "-write-" << fun_name << "-query (protocol seqid args)" << endl;
    indent_up();
    f_service_ <<
      indent() << "\"Encode and send " << fun_name << " request.\"\n" <<
      indent() << "(thrift-protocol-write-message-begin protocol\n" <<
      indent() << "                                     \"" << fun_name << "\"\n" <<
      indent() << "                                     (thrift-constant-message-type 'call)\n" <<
      indent() << "                                     seqid)\n" <<
      indent() << "(thrift-protocol-write-struct-begin protocol \"" << fun_name << "_args\")"<< endl;
    // Encode each argument
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator field_iter;
    for(field_iter=fields.begin(); field_iter!=fields.end(); ++field_iter){
      string arg_name = (*field_iter)->get_name();
      int32_t arg_key = (*field_iter)->get_key();
      string arg_type = "";
      if ((*field_iter)->get_type()->is_base_type()) {
	arg_type = (*field_iter)->get_type()->get_name();
	//arg_type = t_base_name((*field_iter)->get_base());
      } else if ((*field_iter)->get_type()->is_struct()) {
	arg_type = "struct";
      } else if ((*field_iter)->get_type()->is_map()) {
	arg_type = "map";
      } else if ((*field_iter)->get_type()->is_set()) {
	arg_type = "set";
      } else if ((*field_iter)->get_type()->is_enum()) {
	arg_type = "i32";
      }
      f_service_ <<
	indent() << ";; Encode " << arg_name << endl <<
	indent() << "(when (plist-get args :" << arg_name << ")\n" <<
	indent() << "  (thrift-protocol-write-field-begin protocol\n" <<
	indent() << "                                     \"" << arg_name << "\"\n" <<
	indent() << "                                     (thrift-constant-type '" << arg_type << ")\n" <<
	indent() << "                                     " << arg_key << ")" << endl;

      if ((*field_iter)->get_type()->is_base_type()) {
	f_service_ <<
	  indent() << "  (thrift-protocol-write-" << arg_type << " protocol " <<
	  "(plist-get args :" << arg_name << "))" << endl;
      } else {
	f_service_ <<
	  indent() << "  (thrift-gen-" << get_real_el_module((*field_iter)->get_type()->get_program()) <<
	  "-write-" << (*field_iter)->get_type()->get_name() << " protocol" <<
	  " (plist-get args :" << arg_name << "))" << endl;
      }

      f_service_ <<
	indent() << "  (thrift-protocol-write-field-end protocol))" << endl;
    }
    f_service_ <<
      indent() << "(thrift-protocol-write-field-stop protocol)\n" <<
      indent() << "(thrift-protocol-write-struct-end protocol)\n" <<
      indent() << "(thrift-protocol-write-message-end protocol))\n\n" << endl;
    indent_down();

    //
    // generate 'Read' helper function
    //
    if(!(*f_iter)->is_oneway()) {
      string return_type = (*f_iter)->get_returntype()->get_name();
      f_service_ <<
	indent() << "(defun thrift-gen-" << svc_name << "-read-" << fun_name << "-reply (protocol)" << endl;
      indent_up();
      f_service_ <<
	indent() << "\"Receive and decode " << fun_name << " response.\"\n" <<
	indent() << "(let ((res-exception nil)\n" <<
	indent() << "      (res-result nil))\n" <<
	indent() << "  (thrift-protocol-read-struct-begin protocol)\n" <<
	indent() << "  (catch 'break\n" <<
	indent() << "    (while t\n" <<
	indent() << "      (let* ((r (thrift-protocol-read-field-begin protocol))\n" <<
	indent() << "             (fname (pop r))\n" <<
	indent() << "             (ftype (pop r))\n" <<
	indent() << "             (fid (pop r)))\n" <<
	indent() << "        (cond ((equal ftype (thrift-constant-type 'stop))\n" <<
	indent() << "               (throw 'break t))\n";
      if (!(*f_iter)->get_returntype()->is_void()) {
	f_service_ <<
	  indent() << "              ((equal fid 0) ; Normal return value received\n" <<
	  indent() << "               (if (equal ftype (thrift-constant-type '" << return_type << "))\n" <<
	  indent() << "                   (setq res-result (thrift-protocol-read-" << return_type << " protocol))\n" <<
	  indent() << "                 (thrift-protocol-skip protocol ftype)))" << endl;
      }
      // Read/decode exception
      std::vector<t_field*> xceptions = (*f_iter)->get_xceptions()->get_members();
      vector<t_field*>::const_iterator xc_iter;
      for(xc_iter = xceptions.begin(); xc_iter != xceptions.end(); ++xc_iter){
	string xc_name = (*xc_iter)->get_name();
	int32_t xc_key = (*xc_iter)->get_key();
	string xc_type = (*xc_iter)->get_type()->get_name();
	f_service_ <<
	  indent() << "              ((equal fid " << xc_key << ") "
		   << "; " << xc_name << " (" << xc_type << ") exception received\n" <<
	  indent() << "               (if (equal ftype (thrift-constant-type 'struct))\n" <<
	  indent() << "                   (setq res-error (thrift-gen-" << get_real_el_module((*xc_iter)->get_type()->get_program()) << "-read-" << xc_type << " protocol))\n" <<
	  indent() << "                 (thrift-protocol-skip protocol ftype)))" << endl;
      }

      f_service_ <<
	indent() << "              (t\n" <<
	indent() << "               (thrift-protocol-skip protocol ftype)))\n" <<
	indent() << "        (thrift-protocol-read-field-end protocol))))\n" <<
	indent() << "  (thrift-protocol-read-struct-end protocol)\n" <<
	indent() << "  (list res-exception res-result)))\n\n" << endl;
      indent_down();
    }
  }
}


/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_el_generator::generate_service_client(t_service* tservice) {

  string svc_name =  get_real_el_module(tservice->get_program()) + "-" + tservice->get_name();

  // Compute parent class
  string extends_client = "";
  if (tservice->get_extends() != NULL) {
    t_program* program = tservice->get_extends()->get_program();
    extends_client = "(thrift-gen-" + get_real_el_module(program) + "-" + tservice->get_extends()->get_name() + ")";
  } else {
    extends_client = "(thrift-service)";
  }

  // class definition
  f_service_ << endl
    << "(defclass thrift-gen-" << get_real_el_module(tservice->get_program())
    << "-" << tservice->get_name() << " " << extends_client << endl;
  indent_up();
  f_service_ << indent() << "()" << endl;
  f_service_ << indent() << "\"Generated class for the "
	     << get_real_el_module(tservice->get_program()) << "."
	     << tservice->get_name() << " service.\")\n\n" << endl;
  indent_down();

  // Constructor definition
  f_service_
    << "(defmethod initialize-instance ((svc thrift-gen-" << svc_name << ") &rest slots)" << endl;
  indent_up();
  f_service_ << indent() << "\"Create a new instance of a tutorial/Calculator service.\"" << endl;
  f_service_ << indent() << ";; Init parent object" << endl;
  f_service_ << indent() << "(call-next-method)" << endl;
  f_service_ << indent() << ";; Register helper methods" << endl;
  f_service_ << indent() << "(oset svc" << endl;
  f_service_ << indent() << "      functions" << endl;
  f_service_ << indent() << "      (append (oref svc functions)";
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  string open = "'(";
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string fun_name = (*f_iter)->get_name();
    f_service_ << indent() << "\n                " << open << fun_name << endl;
    open = "  ";
    f_service_ <<
      indent() << "                (thrift-gen-" << svc_name << "-write-" << fun_name << "-query\n" <<
      indent() << "                 thrift-gen-" << svc_name << "-read-" << fun_name << "-reply)";
  }
  f_service_ << "))))\n" << endl;
  indent_down();
}


/**
 * Generates a command line tool for making remote requests
 *
 * @param tservice The service to generate a remote for.
 */
void t_el_generator::generate_service_remote(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  //Get all function from parents
  t_service* parent = tservice->get_extends();
  while(parent != NULL) {
    vector<t_function*> p_functions = parent->get_functions();
    functions.insert(functions.end(), p_functions.begin(), p_functions.end());
    parent = parent->get_extends();
  }
  vector<t_function*>::iterator f_iter;

  string f_remote_name = package_dir_+"/"+service_name_+"-remote";
  ofstream f_remote;
  f_remote.open(f_remote_name.c_str());

  f_remote <<
    "#!/usr/bin/env elisp" << endl <<
    el_autogen_comment() << endl <<
    "import sys" << endl <<
    "import pprint" << endl <<
    "from urlparse import urlparse" << endl <<
    "from thrift.transport import TTransport" << endl <<
    "from thrift.transport import TSocket" << endl <<
    "from thrift.transport import THttpClient" << endl <<
    "from thrift.protocol import TBinaryProtocol" << endl <<
    endl;

  f_remote <<
    "from " << module_ << " import " << service_name_ << endl <<
    "from " << module_ << ".ttypes import *" << endl <<
    endl;

  f_remote <<
    "if len(sys.argv) <= 1 or sys.argv[1] == '--help':" << endl <<
    "  print('')" << endl <<
    "  print('Usage: ' + sys.argv[0] + ' [-h host[:port]] [-u url] [-f[ramed]] function [arg1 [arg2...]]')" << endl <<
    "  print('')" << endl <<
    "  print('Functions:')" << endl;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_remote <<
      "  print('  " << (*f_iter)->get_returntype()->get_name() << " " << (*f_iter)->get_name() << "(";
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const std::vector<t_field*>& args = arg_struct->get_members();
    vector<t_field*>::const_iterator a_iter;
    int num_args = args.size();
    bool first = true;
    for (int i = 0; i < num_args; ++i) {
      if (first) {
	first = false;
      } else {
	f_remote << ", ";
      }
      f_remote <<
	args[i]->get_type()->get_name() << " " << args[i]->get_name();
    }
    f_remote << ")')" << endl;
  }
  f_remote <<
    "  print('')" << endl <<
    "  sys.exit(0)" << endl <<
    endl;

  f_remote <<
    "pp = pprint.PrettyPrinter(indent = 2)" << endl <<
    "host = 'localhost'" << endl <<
    "port = 9090" << endl <<
    "uri = ''" << endl <<
    "framed = False" << endl <<
    "http = False" << endl <<
    "argi = 1" << endl <<
    endl <<
    "if sys.argv[argi] == '-h':" << endl <<
    "  parts = sys.argv[argi+1].split(':')" << endl <<
    "  host = parts[0]" << endl <<
    "  if len(parts) > 1:" << endl <<
    "    port = int(parts[1])" << endl <<
    "  argi += 2" << endl <<
    endl <<
    "if sys.argv[argi] == '-u':" << endl <<
    "  url = urlparse(sys.argv[argi+1])" << endl <<
    "  parts = url[1].split(':')" << endl <<
    "  host = parts[0]" << endl <<
    "  if len(parts) > 1:" << endl <<
    "    port = int(parts[1])" << endl <<
    "  else:" << endl <<
    "    port = 80" << endl <<
    "  uri = url[2]" << endl <<
    "  if url[4]:" << endl <<
    "    uri += '?%s' % url[4]" << endl <<
    "  http = True" << endl <<
    "  argi += 2" << endl <<
    endl <<
    "if sys.argv[argi] == '-f' or sys.argv[argi] == '-framed':" << endl <<
    "  framed = True" << endl <<
    "  argi += 1" << endl <<
    endl <<
    "cmd = sys.argv[argi]" << endl <<
    "args = sys.argv[argi+1:]" << endl <<
    endl <<
    "if http:" << endl <<
    "  transport = THttpClient.THttpClient(host, port, uri)" << endl <<
    "else:" << endl <<
    "  socket = TSocket.TSocket(host, port)" << endl <<
    "  if framed:" << endl <<
    "    transport = TTransport.TFramedTransport(socket)" << endl <<
    "  else:" << endl <<
    "    transport = TTransport.TBufferedTransport(socket)" << endl <<
    "protocol = TBinaryProtocol.TBinaryProtocol(transport)" << endl <<
    "client = " << service_name_ << ".Client(protocol)" << endl <<
    "transport.open()" << endl <<
    endl;

  // Generate the dispatch methods
  bool first = true;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_remote << "el";
    }

    t_struct* arg_struct = (*f_iter)->get_arglist();
    const std::vector<t_field*>& args = arg_struct->get_members();
    vector<t_field*>::const_iterator a_iter;
    int num_args = args.size();

    f_remote <<
      "if cmd == '" << (*f_iter)->get_name() << "':" << endl <<
      "  if len(args) != " << num_args << ":" << endl <<
      "    print('" << (*f_iter)->get_name() << " requires " << num_args << " args')" << endl <<
      "    sys.exit(1)" << endl <<
      "  pp.pprint(client." << (*f_iter)->get_name() << "(";
    for (int i = 0; i < num_args; ++i) {
      if (args[i]->get_type()->is_string()) {
	f_remote << "args[" << i << "],";
      } else {
	f_remote << "eval(args[" << i << "]),";
      }
    }
    f_remote << "))" << endl;

    f_remote << endl;
  }

  if (functions.size() > 0) {
    f_remote << "else:" << endl;
    f_remote << "  print('Unrecognized method %s' % cmd)" << endl;
    f_remote << "  sys.exit(1)" << endl;
    f_remote << endl;
  }

  f_remote << "transport.close()" << endl;

  // Close service file
  f_remote.close();

#ifndef _MSC_VER

  // Make file executable, love that bitwise OR action
  chmod(f_remote_name.c_str(),
	  S_IRUSR
	| S_IWUSR
	| S_IXUSR
#ifndef _WIN32
	| S_IRGRP
	| S_IXGRP
	| S_IROTH
	| S_IXOTH
#endif
  );

#endif // _MSC_VER
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_el_generator::generate_service_server(t_service* tservice) {
  // f_service_ << ";;\n;; Here will come the definition of the processor\n;;\n" << endl;
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_el_generator::generate_process_function(t_service* tservice,
					       t_function* tfunction) {
  // (void) tservice;
  // // Open function
  // f_service_ <<
  //   indent() << "def process_" << tfunction->get_name() <<
  //   "(self, seqid, iprot, oprot):" << endl;

  // indent_up();

  // string argsname = tfunction->get_name() + "_args";
  // string resultname = tfunction->get_name() + "_result";

  // f_service_ <<
  //   indent() << "args = " << argsname << "()" << endl <<
  //   indent() << "args.read(iprot)" << endl <<
  //   indent() << "iprot.readMessageEnd()" << endl;

  // t_struct* xs = tfunction->get_xceptions();
  // const std::vector<t_field*>& xceptions = xs->get_members();
  // vector<t_field*>::const_iterator x_iter;

  // // Declare result for non oneway function
  // if (!tfunction->is_oneway()) {
  //   f_service_ <<
  //     indent() << "result = " << resultname << "()" << endl;
  // }

  // // Try block for a function with exceptions
  // if (xceptions.size() > 0) {
  //   f_service_ <<
  //     indent() << "try:" << endl;
  //   indent_up();
  // }

  // // Generate the function call
  // t_struct* arg_struct = tfunction->get_arglist();
  // const std::vector<t_field*>& fields = arg_struct->get_members();
  // vector<t_field*>::const_iterator f_iter;

  // f_service_ << indent();
  // if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()) {
  //   f_service_ << "result.success = ";
  // }
  // f_service_ <<
  //   "self._handler." << tfunction->get_name() << "(";
  // bool first = true;
  // for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
  //   if (first) {
  //     first = false;
  //   } else {
  //     f_service_ << ", ";
  //   }
  //   f_service_ << "args." << (*f_iter)->get_name();
  // }
  // f_service_ << ")" << endl;

  // if (!tfunction->is_oneway() && xceptions.size() > 0) {
  //   indent_down();
  //   for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
  //     f_service_ <<
  //	indent() << "except " << type_name((*x_iter)->get_type()) << ", " << (*x_iter)->get_name() << ":" << endl;
  //     if (!tfunction->is_oneway()) {
  //	indent_up();
  //	f_service_ <<
  //	  indent() << "result." << (*x_iter)->get_name() << " = " << (*x_iter)->get_name() << endl;
  //	indent_down();
  //     } else {
  //	f_service_ <<
  //	  indent() << "pass" << endl;
  //     }
  //   }
  // }

  // // Shortcut out here for oneway functions
  // if (tfunction->is_oneway()) {
  //   f_service_ <<
  //     indent() << "return" << endl;
  //   indent_down();
  //   f_service_ << endl;
  //   return;
  // }

  // f_service_ <<
  //   indent() << "oprot.write-message-begin(\"" << tfunction->get_name() << "\", TMessageType.REPLY, seqid)" << endl <<
  //   indent() << "result.write(oprot)" << endl <<
  //   indent() << "oprot.writeMessageEnd()" << endl <<
  //   indent() << "oprot.trans.flush()" << endl;

  // // Close function
  // indent_down();
  // f_service_ << endl;
}

/**
 * Deserializes a field of any type.
 */
void t_el_generator::generate_deserialize_field(ofstream &out,
						t_field* tfield,
						string prefix,
						bool inclass) {
  (void) inclass;
  t_type* type = get_true_type(tfield->get_type());

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " +
      prefix + tfield->get_name();
  }

  string name = prefix + tfield->get_name();

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out,
				(t_struct*)type,
				 name);
  } else if (type->is_container()) {
    generate_deserialize_container(out, type, name);
  } else if (type->is_base_type() || type->is_enum()) {
    indent(out) <<
      name << " = iprot.";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
	throw "compiler error: cannot serialize void field in a struct: " +
	  name;
	break;
      case t_base_type::TYPE_STRING:
	if (((t_base_type*)type)->is_binary() || !gen_utf8strings_) {
	  out << "readString();";
	} else {
	  out << "readString().decode('utf-8')";
	}
	break;
      case t_base_type::TYPE_BOOL:
	out << "readBool();";
	break;
      case t_base_type::TYPE_BYTE:
	out << "readByte();";
	break;
      case t_base_type::TYPE_I16:
	out << "readI16();";
	break;
      case t_base_type::TYPE_I32:
	out << "readI32();";
	break;
      case t_base_type::TYPE_I64:
	out << "readI64();";
	break;
      case t_base_type::TYPE_DOUBLE:
	out << "readDouble();";
	break;
      default:
	throw "compiler error: no elisp name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "readI32();";
    }
    out << endl;

  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
	   tfield->get_name().c_str(), type->get_name().c_str());
  }
}

/**
 * Generates an unserializer for a struct, calling read()
 */
void t_el_generator::generate_deserialize_struct(ofstream &out,
						  t_struct* tstruct,
						  string prefix) {
//   out <<
//     indent() << prefix << " = " << type_name(tstruct) << "()" << endl <<
//     indent() << prefix << ".read(iprot)" << endl;
}

// /**
//  * Serialize a container by writing out the header followed by
//  * data and then a footer.
//  */
void t_el_generator::generate_deserialize_container(ofstream &out,
						    t_type* ttype,
						    string prefix) {
//   string size = tmp("_size");
//   string ktype = tmp("_ktype");
//   string vtype = tmp("_vtype");
//   string etype = tmp("_etype");

//   t_field fsize(g_type_i32, size);
//   t_field fktype(g_type_byte, ktype);
//   t_field fvtype(g_type_byte, vtype);
//   t_field fetype(g_type_byte, etype);

//   // Declare variables, read header
//   if (ttype->is_map()) {
//     out <<
//       indent() << prefix << " = {}" << endl <<
//       indent() << "(" << ktype << ", " << vtype << ", " << size << " ) = iprot.readMapBegin()" << endl;
//   } else if (ttype->is_set()) {
//     out <<
//       indent() << prefix << " = set()" << endl <<
//       indent() << "(" << etype << ", " << size << ") = iprot.readSetBegin()" << endl;
//   } else if (ttype->is_list()) {
//     out <<
//       indent() << prefix << " = []" << endl <<
//       indent() << "(" << etype << ", " << size << ") = iprot.readListBegin()" << endl;
//   }

//   // For loop iterates over elements
//   string i = tmp("_i");
//   indent(out) <<
//     "for " << i << " in xrange(" << size << "):" << endl;

//     indent_up();

//     if (ttype->is_map()) {
//       generate_deserialize_map_element(out, (t_map*)ttype, prefix);
//     } else if (ttype->is_set()) {
//       generate_deserialize_set_element(out, (t_set*)ttype, prefix);
//     } else if (ttype->is_list()) {
//       generate_deserialize_list_element(out, (t_list*)ttype, prefix);
//     }

//     indent_down();

//   // Read container end
//   if (ttype->is_map()) {
//     indent(out) << "iprot.readMapEnd()" << endl;
//   } else if (ttype->is_set()) {
//     indent(out) << "iprot.readSetEnd()" << endl;
//   } else if (ttype->is_list()) {
//     indent(out) << "iprot.readListEnd()" << endl;
//   }
}


/**
 * Generates code to deserialize a map
 */
void t_el_generator::generate_deserialize_map_element(ofstream &out,
						       t_map* tmap,
						       string prefix) {
  // string key = tmp("_key");
  // string val = tmp("_val");
  // t_field fkey(tmap->get_key_type(), key);
  // t_field fval(tmap->get_val_type(), val);

  // generate_deserialize_field(out, &fkey);
  // generate_deserialize_field(out, &fval);

  // indent(out) <<
  //   prefix << "[" << key << "] = " << val << endl;
}

/**
 * Write a set element
 */
void t_el_generator::generate_deserialize_set_element(ofstream &out,
						       t_set* tset,
						       string prefix) {
  // string elem = tmp("_elem");
  // t_field felem(tset->get_elem_type(), elem);

  // generate_deserialize_field(out, &felem);

  // indent(out) <<
  //   prefix << ".add(" << elem << ")" << endl;
}

/**
 * Write a list element
 */
void t_el_generator::generate_deserialize_list_element(ofstream &out,
							t_list* tlist,
							string prefix) {
  // string elem = tmp("_elem");
  // t_field felem(tlist->get_elem_type(), elem);

  // generate_deserialize_field(out, &felem);

  // indent(out) <<
  //   prefix << ".append(" << elem << ")" << endl;
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_el_generator::generate_serialize_field(ofstream &out,
					       t_field* tfield,
					       string prefix) {
  // t_type* type = get_true_type(tfield->get_type());

  // // Do nothing for void types
  // if (type->is_void()) {
  //   throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " +
  //     prefix + tfield->get_name();
  // }

  // if (type->is_struct() || type->is_xception()) {
  //   generate_serialize_struct(out,
  //			      (t_struct*)type,
  //			      prefix + tfield->get_name());
  // } else if (type->is_container()) {
  //   generate_serialize_container(out,
  //				 type,
  //				 prefix + tfield->get_name());
  // } else if (type->is_base_type() || type->is_enum()) {

  //   string name = prefix + tfield->get_name();

  //   indent(out) <<
  //     "oprot.";

  //   if (type->is_base_type()) {
  //     t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
  //     switch (tbase) {
  //     case t_base_type::TYPE_VOID:
  //	throw
  //	  "compiler error: cannot serialize void field in a struct: " + name;
  //	break;
  //     case t_base_type::TYPE_STRING:
  //	if (((t_base_type*)type)->is_binary() || !gen_utf8strings_) {
  //	  out << "writeString(" << name << ")";
  //	} else {
  //	  out << "writeString(" << name << ".encode('utf-8'))";
  //	}
  //	break;
  //     case t_base_type::TYPE_BOOL:
  //	out << "writeBool(" << name << ")";
  //	break;
  //     case t_base_type::TYPE_BYTE:
  //	out << "writeByte(" << name << ")";
  //	break;
  //     case t_base_type::TYPE_I16:
  //	out << "writeI16(" << name << ")";
  //	break;
  //     case t_base_type::TYPE_I32:
  //	out << "writeI32(" << name << ")";
  //	break;
  //     case t_base_type::TYPE_I64:
  //	out << "writeI64(" << name << ")";
  //	break;
  //     case t_base_type::TYPE_DOUBLE:
  //	out << "writeDouble(" << name << ")";
  //	break;
  //     default:
  //	throw "compiler error: no Elisp name for base type " + t_base_type::t_base_name(tbase);
  //     }
  //   } else if (type->is_enum()) {
  //     out << "writeI32(" << name << ")";
  //   }
  //   out << endl;
  // } else {
  //   printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s%s' TYPE '%s'\n",
  //	   prefix.c_str(),
  //	   tfield->get_name().c_str(),
  //	   type->get_name().c_str());
  // }
}

/**
 * Serializes all the members of a struct.
 *
 * @param tstruct The struct to serialize
 * @param prefix  String prefix to attach to all fields
 */
void t_el_generator::generate_serialize_struct(ofstream &out,
					       t_struct* tstruct,
					       string prefix) {
  // (void) tstruct;
  // indent(out) << prefix << ".write(oprot)" << endl;
}

void t_el_generator::generate_serialize_container(ofstream &out,
						  t_type* ttype,
						  string prefix) {
  // if (ttype->is_map()) {
  //   indent(out) <<
  //     "oprot.writeMapBegin(" <<
  //     type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
  //     type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
  //     "len(" << prefix << "))" << endl;
  // } else if (ttype->is_set()) {
  //   indent(out) <<
  //     "oprot.writeSetBegin(" <<
  //     type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
  //     "len(" << prefix << "))" << endl;
  // } else if (ttype->is_list()) {
  //   indent(out) <<
  //     "oprot.writeListBegin(" <<
  //     type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
  //     "len(" << prefix << "))" << endl;
  // }

  // if (ttype->is_map()) {
  //   string kiter = tmp("kiter");
  //   string viter = tmp("viter");
  //   indent(out) <<
  //     "for " << kiter << "," << viter << " in " << prefix << ".items():" << endl;
  //   indent_up();
  //   generate_serialize_map_element(out, (t_map*)ttype, kiter, viter);
  //   indent_down();
  // } else if (ttype->is_set()) {
  //   string iter = tmp("iter");
  //   indent(out) <<
  //     "for " << iter << " in " << prefix << ":" << endl;
  //   indent_up();
  //   generate_serialize_set_element(out, (t_set*)ttype, iter);
  //   indent_down();
  // } else if (ttype->is_list()) {
  //   string iter = tmp("iter");
  //   indent(out) <<
  //     "for " << iter << " in " << prefix << ":" << endl;
  //   indent_up();
  //   generate_serialize_list_element(out, (t_list*)ttype, iter);
  //   indent_down();
  // }

  // if (ttype->is_map()) {
  //   indent(out) <<
  //     "oprot.writeMapEnd()" << endl;
  // } else if (ttype->is_set()) {
  //   indent(out) <<
  //     "oprot.writeSetEnd()" << endl;
  // } else if (ttype->is_list()) {
  //   indent(out) <<
  //     "oprot.writeListEnd()" << endl;
  // }
}

/**
 * Serializes the members of a map.
 *
 */
void t_el_generator::generate_serialize_map_element(ofstream &out,
						     t_map* tmap,
						     string kiter,
						     string viter) {
  // t_field kfield(tmap->get_key_type(), kiter);
  // generate_serialize_field(out, &kfield, "");

  // t_field vfield(tmap->get_val_type(), viter);
  // generate_serialize_field(out, &vfield, "");
}

/**
 * Serializes the members of a set.
 */
void t_el_generator::generate_serialize_set_element(ofstream &out,
						     t_set* tset,
						     string iter) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(out, &efield, "");
}

/**
 * Serializes the members of a list.
 */
void t_el_generator::generate_serialize_list_element(ofstream &out,
						      t_list* tlist,
						      string iter) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(out, &efield, "");
}

/**
 * Generates the docstring for a given struct.
 */
void t_el_generator::generate_elisp_docstring(ofstream& out,
					       t_struct* tstruct) {
  generate_elisp_docstring(out, tstruct, tstruct, "Attributes");
}

/**
 * Generates the docstring for a given function.
 */
void t_el_generator::generate_elisp_docstring(ofstream& out,
					       t_function* tfunction) {
  generate_elisp_docstring(out, tfunction, tfunction->get_arglist(), "Parameters");
}

/**
 * Generates the docstring for a struct or function.
 */
void t_el_generator::generate_elisp_docstring(ofstream& out,
					       t_doc*    tdoc,
					       t_struct* tstruct,
					       const char* subheader) {
  bool has_doc = false;
  stringstream ss;
  if (tdoc->has_doc()) {
    has_doc = true;
    ss << tdoc->get_doc();
  }

  const vector<t_field*>& fields = tstruct->get_members();
  if (fields.size() > 0) {
    if (has_doc) {
      ss << endl;
    }
    has_doc = true;
    ss << subheader << ":\n";
    vector<t_field*>::const_iterator p_iter;
    for (p_iter = fields.begin(); p_iter != fields.end(); ++p_iter) {
      t_field* p = *p_iter;
      ss << " - " << p->get_name();
      if (p->has_doc()) {
	ss << ": " << p->get_doc();
      } else {
	ss << endl;
      }
    }
  }

  if (has_doc) {
    generate_docstring_comment(out,
      ";;\n",
      ";; ", ss.str(),
      ";;\n");
  }
}

/**
 * Generates the docstring for a generic object.
 */
void t_el_generator::generate_elisp_docstring(ofstream& out,
					       t_doc* tdoc) {
  if (tdoc->has_doc()) {
    generate_docstring_comment(out,
      ";;\n",
      ";; ", tdoc->get_doc(),
      ";;\n");
  }
}

/**
 * Declares an argument, which may include initialization as necessary.
 *
 * @param tfield The field
 */
string t_el_generator::declare_argument(t_field* tfield) {
  // std::ostringstream result;
  // result << tfield->get_name() << "=";
  // if (tfield->get_value() != NULL) {
  //   result << "thrift_spec[" <<
  //     tfield->get_key() << "][4]";
  // } else {
  //   result << "None";
  // }
  // return result.str();
  return "";
}

/**
 * Renders a field default value, returns None otherwise.
 *
 * @param tfield The field
 */
string t_el_generator::render_field_default_value(t_field* tfield) {
  // t_type* type = get_true_type(tfield->get_type());
  // if (tfield->get_value() != NULL) {
  //   return render_const_value(type, tfield->get_value());
  // } else {
  //   return "None";
  // }
  return "";
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_el_generator::function_signature(t_function* tfunction) {
  // vector<string> pre;
  // vector<string> post;
  // string signature = "(" + tfunction->get_name();

  // signature += argument_list(tfunction->get_arglist(), &pre, &post) + ")";
  // return signature;
  return "";
}

/**
 * Renders a field list
 */
string t_el_generator::argument_list(t_struct* tstruct, vector<string> *pre, vector<string> *post) {
  string result = "";

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  vector<string>::const_iterator s_iter;
  bool first = true;
  if (pre) {
    for (s_iter = pre->begin(); s_iter != pre->end(); ++s_iter) {
      if (first) {
	first = false;
      } else {
	result += " ";
      }
      result += *s_iter;
    }
  }
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      result += " ";
    }
    result += (*f_iter)->get_name();
  }
  if (post) {
    for (s_iter = post->begin(); s_iter != post->end(); ++s_iter) {
      if (first) {
	first = false;
      } else {
	result += " ";
      }
      result += *s_iter;
    }
  }
  return result;
}

string t_el_generator::type_name(t_type* ttype) {
  t_program* program = ttype->get_program();
  if (ttype->is_service()) {
    return get_real_el_module(program) + "." + ttype->get_name();
  }
  if (program != NULL && program != program_) {
    return get_real_el_module(program) + ".ttypes." + ttype->get_name();
  }
  return ttype->get_name();
}

/**
 * Converts the parse type to a Elisp type
 */
string t_el_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "TType.STRING";
    case t_base_type::TYPE_BOOL:
      return "TType.BOOL";
    case t_base_type::TYPE_BYTE:
      return "TType.BYTE";
    case t_base_type::TYPE_I16:
      return "TType.I16";
    case t_base_type::TYPE_I32:
      return "TType.I32";
    case t_base_type::TYPE_I64:
      return "TType.I64";
    case t_base_type::TYPE_DOUBLE:
      return "TType.DOUBLE";
    }
  } else if (type->is_enum()) {
    return "TType.I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "TType.STRUCT";
  } else if (type->is_map()) {
    return "TType.MAP";
  } else if (type->is_set()) {
    return "TType.SET";
  } else if (type->is_list()) {
    return "TType.LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

/** See the comment inside generate_el_struct_definition for what this is. */
// string t_el_generator::type_to_spec_args(t_type* ttype) {
//   while (ttype->is_typedef()) {
//     ttype = ((t_typedef*)ttype)->get_type();
//   }

//   if (ttype->is_base_type() || ttype->is_enum()) {
//     return "None";
//   } else if (ttype->is_struct() || ttype->is_xception()) {
//     return "(" + type_name(ttype) + ", " + type_name(ttype) + ".thrift_spec)";
//   } else if (ttype->is_map()) {
//     return "(" +
//       type_to_enum(((t_map*)ttype)->get_key_type()) + "," +
//       type_to_spec_args(((t_map*)ttype)->get_key_type()) + "," +
//       type_to_enum(((t_map*)ttype)->get_val_type()) + "," +
//       type_to_spec_args(((t_map*)ttype)->get_val_type()) +
//       ")";

//   } else if (ttype->is_set()) {
//     return "(" +
//       type_to_enum(((t_set*)ttype)->get_elem_type()) + "," +
//       type_to_spec_args(((t_set*)ttype)->get_elem_type()) +
//       ")";

//   } else if (ttype->is_list()) {
//     return "(" +
//       type_to_enum(((t_list*)ttype)->get_elem_type()) + "," +
//       type_to_spec_args(((t_list*)ttype)->get_elem_type()) +
//       ")";
//   }

//   throw "INVALID TYPE IN type_to_spec_args: " + ttype->get_name();
// }


THRIFT_REGISTER_GENERATOR(el, "Elisp", "")
