package u04lab.code

import u04lab.code.Lists._ // import custom List type (not the one in Scala stdlib)

trait Student {
  def name: String
  def year: Int
  def enrolling(course: Course): Unit // the student participates to a Course
  def enrolling2(course: Course*): Unit // refactor the code so that method enrolling accepts a variable argument Course*
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?
}

trait Course {
  def name: String
  def teacher: String
}

object Student {
  def apply(name: String, year: Int = 2017): Student = new StudentImpl(name, year)
  def apply(name: String): Student = new StudentImpl(name, 2017)

  private class StudentImpl( override val name: String,
                             override val year: Int) extends Student {

   private var coursesList: List[Course] = List.nil

    override def enrolling(course: Course): Unit = coursesList = List.append(coursesList, List.Cons(course, List.nil))

    override def enrolling2(courses: Course*): Unit = {
      for (c <- courses) {
        coursesList = List.append(coursesList, List.Cons(c, List.nil))
      }
    }
    override def courses: List[String] = List.map(coursesList)(v => v.name)

    override def hasTeacher(teacher: String): Boolean = List.contains(List.map(coursesList)(v => v.teacher))(teacher)
  }
}

object Course {
  def apply(name: String, teacher: String): Course = new CourseImpl(name, teacher)

  def unapply(c: Course): Option[(String,String)] = Some((c.name,c.teacher)) // Optional Exercise

  private class CourseImpl( override val name: String,
                             override val teacher: String) extends Course {

  }
}

/* Version with using case class */
/*
case class CourseImpl(override val name: String,
                      override val teacher: String) extends Course {

}

case class StudentImpl(override val name: String,
                       override val year: Int) extends Student{
  private var coursesList: List[Course] = List.nil

  override def enrolling(course: Course): Unit = {
    coursesList = List.append(coursesList, List.Cons(course, List.nil))
  }

  override def courses: List[String] = List.map(coursesList)(v => v.name)

  override def hasTeacher(teacher: String): Boolean = List.contains(List.map(coursesList)(v => v.teacher))(teacher)
}
*/

object Try extends App {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017
  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)
  println(s1.courses, s2.courses, s3.courses) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true
  /* enrolling2 accepts a variable argument Course */
  s1.enrolling2(cPPS, cPCD)
  s2.enrolling2(cPPS)
  s3.enrolling2(cPPS, cPCD, cSDR)
}

/** Hints:
  * - simply implement Course, e.g. with a case class
  * - implement Student with a StudentImpl keeping a private Set of courses
  * - try to implement in StudentImpl method courses with map
  * - try to implement in StudentImpl method hasTeacher with map and find
  * - check that the two println above work correctly
  * - refactor the code so that method enrolling accepts a variable argument Course*
  */
