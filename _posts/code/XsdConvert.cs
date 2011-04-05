using System;
using System.Diagnostics;
using System.Xml;
using System.Xml.Serialization;

public class XmlValueWrapper
{
    private object value;

    public object Value
    {
        get { return value; }
        set { this.value = value; }
    }
}

public static class XsdConvert
{
    private static XmlSerializer serializer = new XmlSerializer(typeof(XmlValueWrapper));

    public static object ConvertFrom(string value, string xsdType)
    {
        XmlDocument doc = new XmlDocument();
        XmlElement rootElement = (XmlElement) doc.AppendChild(doc.CreateElement("XmlValueWrapper"));
        rootElement.SetAttribute("xmlns:xs", "http://www.w3.org/2001/XMLSchema");

        XmlElement valueElement = (XmlElement) rootElement.AppendChild(doc.CreateElement("Value"));
        valueElement.SetAttribute("type", "http://www.w3.org/2001/XMLSchema-instance", xsdType);
        valueElement.AppendChild(doc.CreateTextNode(value));

        using (XmlNodeReader reader = new XmlNodeReader(doc))
        {
            XmlValueWrapper wrapper = (XmlValueWrapper) serializer.Deserialize(reader);
            return wrapper.Value;
        }
    }
}

public static class Program
{
    public static void Main()
    {
        Debug.Assert(Equals(42, XsdConvert.ConvertFrom("42", "xs:int")));
        Debug.Assert(Equals(42.0, XsdConvert.ConvertFrom("42", "xs:double")));
        Debug.Assert(Equals(42m, XsdConvert.ConvertFrom("42", "xs:decimal")));
        Debug.Assert(Equals("42", XsdConvert.ConvertFrom("42", "xs:string")));
        Debug.Assert(Equals(true, XsdConvert.ConvertFrom("true", "xs:boolean")));
        Debug.Assert(Equals(new DateTime(2009, 4, 17), XsdConvert.ConvertFrom("2009-04-17", "xs:date")));
    }
}
