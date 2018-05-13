import java.io.BufferedWriter
import java.io.FileWriter

object CopybookParser {


  val field = """(\d{2})\s+(\S+)""".r
  val occurs="""\s+OCCURS (\d+) TIMES""".r
  val index = """\s+INDEXED BY\s(\S+)""".r
  val redefines="""\s+REDEFINES\s(\S+)""".r
  val pic="""\s+PIC\s+(\S+)(\s+(\S+))?""".r
  val end="""\.$""".r
  val typlen="""(.*)\((\d+)\).*""".r
  //regex union in scala
  //https://stackoverflow.com/questions/13822757/scala-regex-union
val entry=(field.pattern.pattern + "(" + occurs.pattern.pattern + ")?(" + index.pattern.pattern + ")?(" +  redefines.pattern.pattern + ")?(" + pic.pattern.pattern + ")?"+ end.pattern.pattern).r



  def main(args: Array[String]): Unit = {

    val inputFile=args(0)
    val outputFile=args(1)
    val Copybooktoseq= scala.io.Source.fromFile(inputFile).getLines.toSeq
    val writer = new BufferedWriter(new FileWriter(outputFile))

    //clean up comments and trim front and back sequence numbers in copybook
    val CleanedCb= Copybooktoseq.filter(el => el.length>6 && el.charAt(6)!='*').map(el => if (el.length > 72) el.substring(6,72) else el.substring(6,el.length)).map(_.replaceAll("^\\s+",""))

    //group all multi-line entries ending with '.' in a single line.
    var prev = ""
    var temp = ""
    val SinglelineCb=CleanedCb.foldLeft(List[String]()) {
      case (List(),cur) => List(cur)
      case (ls,cur) if cur.contains(".") => { temp= prev + " " + cur; prev=" ";ls :+ temp}
      case (ls,cur) => {prev=prev+cur;ls}
    }

    //filter out 88 line entries for now
    val Filter88Cb = SinglelineCb.map(_.replaceAll("^\\s+","")).filter(!_.matches("""^88.*""")).map(_.replaceAll("\\s+$",""))

    //parse each entry based on the regular expressions.
    case class CopyBookEntry(level:Int,field:String,occurs:String,index:String,redefines:String,pic:String,packed:String)
    val ParsedCb= Filter88Cb.map(el => {
      el match {
        case entry(level,field,occursgrp,occurs,indexgrp,index,redefinesgrp,redefines,picgrp,pic,space,packed) =>  CopyBookEntry(level.toInt,field,occurs,index,redefines,pic,packed)
        case _ => CopyBookEntry(0,null,null,null,null,null,null)
      }
    })


    //Find redefined fields and remove corresponding field entries
    var baselevel=0
    val RedefinedFields = ParsedCb.map(CBE => if (CBE.redefines != null) CBE.redefines else "none").filter(e => e!="none")
    val RedefFieldsNestedToo = ParsedCb.foldLeft(List[String]()) {
      case (List(),cur) if RedefinedFields.exists(rf=> rf == cur.field) => {baselevel=cur.level; List (cur.field)}
      case (List(),cur) => List()
      case (ls,cur) if RedefinedFields.exists(rf=> rf == cur.field) => {baselevel=cur.level; ls :+ cur.field}
      case (ls,cur) if cur.level>baselevel =>{ ls :+ cur.field}
      case (ls,cur) =>  {ls}
    }
    val RedefineRemovedCb=ParsedCb.filter(cur => !RedefFieldsNestedToo.exists(rf=> rf == cur.field))

    //Repeat Occur entries as appropriate
    //In the first pass, repeat entries with both occurs and pics
    val OccursPicCb= RedefineRemovedCb.foldLeft(List[CopyBookEntry]()) {
      case (List(),cur) => List(cur)
      case (ls,cur) if cur.occurs !=null && cur.pic!=null => {
        val occurslist= for (i <- 1 to cur.occurs.toInt) yield cur.copy(field = cur.field +"-"+i,occurs=null)
        ls ++ occurslist
      }
      case (ls,cur) => ls:+cur
    }
    //In the second pass repeat occurs groups. will not handle nested groups
    var occursGrplist=List[CopyBookEntry]()
    var occursGrplevel=0
    var occursTimes=0
    val OccursGrpCb= OccursPicCb.foldLeft(List[CopyBookEntry]()) {
      case (List(),cur) => List(cur)
      case (ls,cur) if cur.occurs !=null  => { occursGrplevel=cur.level;occursTimes=cur.occurs.toInt;ls}
      case (ls,cur) if occursTimes!=0 && cur.level>occursGrplevel => {occursGrplist=occursGrplist :+ cur;ls }
      case (ls,cur) if occursTimes!=0 && cur.level<=occursGrplevel=> {
        val occursGrpRepeatlist= for (i <- 1 to occursTimes;el <- occursGrplist) yield el.copy(field = el.field +"-"+i)
        occursGrplist=List[CopyBookEntry]()
        occursGrplevel=0
        occursTimes=0
        ls ++ occursGrpRepeatlist :+ cur
      }
      case (ls,cur) => ls :+ cur
    }
   // Field Label, Datatype, length
   //999,99,9 unsigned s9,s999,s99v9,s9v99 signed, X,XXX,XX alphanumeric
   // Report the processed field labels and types into output file
    OccursGrpCb.filter(_.pic != null).map(cbe => {
      cbe.pic match {
        case typlen(typ, len) => {
          val datatype = if (typ.contains('X')) "Alphanumeric" else "Numeric"
          val signed= if (datatype == "Alphanumeric") "NA" else if (typ.contains('S')) "Signed" else "Unsigned"
          val packed = if (datatype == "Alphanumeric") "NA" else if (cbe.packed == "COMP-3") "Packed" else "Binary"
          val datalen = if (len != null) len else typ.length
          cbe.field + " , " + datatype + " , " + signed + " , " + packed + " , " + datalen + " \n"
        }
      }
    }).foreach(writer.write)
    writer.close()




  }
}
