package net.ivoah.sqala

import utest.*

import java.sql.{ResultSet, SQLException}

class SQLTests extends TestSuite {
  case class Person(name: String, age: Int)
  object Person {
    def fromResultSet(r: ResultSet) = Person(r.getString("name"), r.getInt("age"))
  }

  def withTestDb(fn: Connector ?=> Unit) = {
    given db: Connector = Connector("jdbc:sqlite::memory:")
    sql"CREATE TABLE person (name text, age integer)".execute()
    sql"""INSERT INTO person (name, age) VALUES ("Noah", 28)""".update()
    fn
    db.close()
  }

  val tests = Tests {
    test("create") {
      withTestDb {
        sql"CREATE TABLE person2 (name text, age integer)".execute()
      }
    }

    test("insert") {
      withTestDb {
        val person = Person("Lydia", 25)
        assert(sql"INSERT INTO person (name, age) VALUES (${person.name}, ${person.age})".update() == 1)
      }
    }

    test("select") {
      withTestDb {
        val result = sql"SELECT * FROM person".query(Person.fromResultSet)
        assert(result == Seq(Person("Noah", 28)))
      }
    }

    test("raw query (wrong)") {
      withTestDb {
        val tableName = "person"
        assertThrows[SQLException] {
          sql"SELECT * FROM ${tableName}".query(Person.fromResultSet)
        }
      }
    }

    test("raw query (correct)") {
      withTestDb {
        val tableName = "person"
        assert(sql"SELECT * FROM ${tableName.sql}".query(Person.fromResultSet).length == 1)
      }
    }

    test("injection protection") {
      withTestDb {
        val where = """Noah"; DROP TABLE person"""
        assert(sql"SELECT * FROM person WHERE name LIKE $where".query(Person.fromResultSet).length == 0)
        assert(sql"SELECT * FROM person".query(Person.fromResultSet).length == 1)
      }
    }
  }
}
