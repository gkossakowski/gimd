case class User(name: String, email: String, age: Int)
object MyUserType extends com.google.gimd.UserType[User] {
  //define binding between Messages that Gimd stores and some custom class
  import com.google.gimd._
  import com.google.gimd.UserType._
  val name  = FieldSpecOne("name", StringField, _.name)
  val email = FieldSpecOne("email", StringField, _.email)
  val age = FieldSpecOne("age", IntField, _.age)
  val fields = name :: email :: age :: Nil
  def toUserObject(m: Message) = User(name(m), email(m), age(m))
}
object MyUserFileType extends com.google.gimd.file.FileType[User] {
  //define file type which specifices how to store Messages corresponding to given UserType
  import com.google.gimd.Message
  import com.google.gimd.UserType._
  val pathPrefix = Some("user/")
  val pathSuffix = None
  val userType = MyUserType
  def name(m: Message) = userType.name(m)
}

import com.google.gimd._

val db = GimdConsole.openDb("sample-repo", MyUserFileType :: Nil)
//executed in single transaction
db.modifyAndReturn { s: Snapshot =>
  //query without where clause matches everything
  val users = s.query(MyUserFileType, MyUserType.query)
  //if there are no users in our db then let's add some!
  if (users.isEmpty) {
    val newUsers = for (i <- 1 to 1000) yield User("user"+i, "email"+i+"@com.com", i%91)
    (newUsers.foldLeft(modification.DatabaseModification.empty) {
      case (m, user) => m.insertFile(MyUserFileType, user)
    }, "added new users")
  } else
    (modification.DatabaseModification.empty, "didn't modify anything")
}
import query.Query._
println("-- Querying for users with age less than 3")
db.query(MyUserFileType, MyUserType.query where { _.age < 3 }) foreach println
println("-- Querying for user with name 'user78'")
db.query(MyUserFileType, MyUserType.query where { _.name === "user78" }) foreach println
