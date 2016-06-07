/*
Copyright (C) 2010, Ferruccio Barletta (ferruccio.barletta@gmail.com)

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*/
#ifndef IGLOO_XMLWRITER_HPP
#define IGLOO_XMLWRITER_HPP

#include <string>
#include <iostream>
#include <sstream>
#include <stack>
#include <cassert>

namespace igloo {
  class XmlElement;

  class XmlWriter
  {
  public:
      // XmlWriter must be bound to an ostream
      XmlWriter(std::ostream& os_) : os(os_) {
        os << "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
      }
      ~XmlWriter(void) { assert(elements.empty()); }

  private:
      std::ostream& os;               // output stream
      std::stack<XmlElement*> elements;  // stack of open element tags

      // write a single character to the output stream
      XmlWriter& putc(char c) {
          os.put(c);
          return *this;
      }

      // write a string to the output stream
      XmlWriter& puts(const char* str) {
          os << str;
          return *this;
      }

      friend class XmlElement;
  };

  //
  //	XML element class
  //
  class XmlElement
  {
  public:
      // create a new element tag, bound to an XmlWriter
      XmlElement(const char* name_, XmlWriter& wr_) : name(name_), wr(wr_) {
          assert(name != 0);
          check_parent();
          wr.putc('<').puts(name);
          tagopen = true;
          wr.elements.push(this);
      }

      // close the current element tag
      ~XmlElement() {
          if (!wr.elements.empty() && wr.elements.top() == this) {
              wr.elements.pop();
              if (tagopen)
                  wr.puts("/>");
              else
                  wr.puts("</").puts(name).putc('>');
          }
      }

      // write an attribute for the current element
      XmlElement& attr(const char* attr_name, const char* value) {
          assert(attr_name != 0);
          assert(value != 0);
          assert(tagopen);
          wr.putc(' ').puts(attr_name).puts("=\"");
          qputs(value);
          wr.putc('"');
          return *this;
      }

      // attr() overload for std::string type
      XmlElement& attr(const char* attr_name, const std::string& value) { 
		  return attr(attr_name, value.c_str());
	  }

      // attr() function template for all streamable types
      template <class T>
      XmlElement& attr(const char* attr_name, T value) {
          std::stringstream ss;
          ss << value;
          attr(attr_name, ss.str());
          return *this;
      }

      // write text content for the current element
      XmlElement& contents(const char* str) {
          assert(str != 0);
          check_parent();
          qputs(str);
          return *this;
      }

      // contents() overload for std::string type
      XmlElement& contents(const std::string& str) {
		  return contents(str.c_str());
	  }

      // contents() function template for all streamable types
      template <class T>
      XmlElement& contents(T value) {
          std::stringstream ss;
          ss << value;
          contents(ss.str());
          return *this;
      }

      // write CDATA section
      XmlElement& cdata(const char* str) {
          assert(str != 0);
          check_parent();
          wr.puts("<![CDATA[");
          wr.puts(str);
          wr.puts("]]>");
          return *this;
      }

      // cdata() overload for std::string type
      XmlElement& cdata(const std::string& str) {
		  return cdata(str.c_str());
	  }
  private:
      const char* name;   // name of current element
      XmlWriter& wr;         // bound XML writer
      bool tagopen;       // is the element tag for this element still open?

      // write a string quoting characters which have meaning in xml
      XmlElement& qputs(const char* str) {
          for (; *str; ++str)
              switch (*str) {
                  case '&': wr.puts("&amp;"); break;
                  case '<': wr.puts("&lt;"); break;
                  case '>': wr.puts("&gt;"); break;
                  case '\'': wr.puts("&apos;"); break;
                  case '"': wr.puts("&quot;"); break;
                  default: wr.putc(*str); break;
              }
          return *this;
      }

      // check to see if we have a parent tag which needs to be closed
      void check_parent() {
          if (!wr.elements.empty() && wr.elements.top()->tagopen) {
              wr.putc('>');
              wr.elements.top()->tagopen = false;
          }
      }
  };
}

#endif
