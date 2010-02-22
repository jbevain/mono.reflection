using System;
using System.Reflection;
using System.Linq;

static class TypeRocks {

	public static PropertyInfo GetIndexer (this Type self)
	{
		return GetIndexer (self, GetIndexers (self));
	}

	public static PropertyInfo GetIndexer (this Type self, BindingFlags flags)
	{
		return GetIndexer (self, GetIndexers (self, flags));
	}

	static PropertyInfo GetIndexer (Type self, PropertyInfo [] indexers)
	{
		switch (indexers.Length) {
		case 0:
			return null;
		case 1:
			return indexers [0];
		default:
			throw new AmbiguousMatchException ();
		}
	}

	public static PropertyInfo [] GetIndexers (this Type self)
	{
		return GetIndexers (self, self.GetProperties ());
	}

	public static PropertyInfo [] GetIndexers (this Type self, BindingFlags flags)
	{
		return GetIndexers (self, self.GetProperties (flags));
	}

	static PropertyInfo [] GetIndexers (Type self, PropertyInfo [] properties)
	{
		var indexer_name = GetIndexerName (self);
		if (indexer_name.Length == 0)
			return new PropertyInfo [0];

		return properties.Where (property => property.Name == indexer_name).ToArray ();
	}

	static string GetIndexerName (this Type self)
	{
		if (self == null)
			throw new ArgumentNullException ("self");

		var attribute = (DefaultMemberAttribute) Attribute.GetCustomAttribute (self, typeof (DefaultMemberAttribute));
		if (attribute == null)
			return string.Empty;

		return attribute.MemberName;
	}
}
