package backend.mangler;

case class NameMapper(table: Map[String, String], parent: Option[NameMapper], current: Int) {
	def getParent(): Option[NameMapper] = parent
	
	def getTable(): Map[String, String] = table

	def addName(name: String) : (NameMapper,String) = {
		val newName: String = "_" + current.toString
		val newRoot: Map[String, String] = table + (name -> newName)
		(NameMapper(newRoot, parent, current+1),newName)
	}

	def openScope() : NameMapper = {
		NameMapper(Map(), Some(this), current)
	}

	def closeScope() : Option[NameMapper] = {
		parent.map(p => NameMapper(p.getTable, p.getParent, current))
	}

	def getName(name: String) : Option[String] = getFromMap(name) match {
		case Some(str) => Some(str)
		case None => parent match {
			case Some(mapper) => mapper.getName(name)
			case None => None
		}
	}
	
	def getFromMap(name: String) : Option[String] = {
		try { Some(table(name)) } 
		catch { case e:Exception => None }
	}
}