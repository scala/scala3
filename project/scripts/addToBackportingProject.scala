//> using scala 3.lts
//> using toolkit 0.4.0
//> using lib pro.kordyjan::pytanie:0.1.9

import pytanie.*
import sttp.client4.*

lazy val apiToken =
  System.getenv("GRAPHQL_API_TOKEN")

case class ID(value: String) derives WrapperVariable

// Obtained with:
// query {
//   organization(login: "scala") {
//     projectV2(number: 2) {
//       id
//     }
//   }
// }
val PROJECT_ID = ID("PVT_kwDN3uPOAHewkg")

// Obtained with:
// query {
//   organization(login: "scala") {
//     projectV2(number: 2) {
//       field(name: "Merged at") {
//         ... on ProjectV2FieldCommon {
//           id
//         }
//       }
//     }
//   }
// }
val FIELD_ID = ID("PVTF_lADN3uPOAHewks4E3B1I")

@main def run(commitSha: String) =
  val (id, date) = getPrData(commitSha)
  val newId = addItem(id)
  timestampItem(newId, date)

def getPrData(commitSha: String): (ID, String) =
  val res = query"""
    |query prForCommit {
    |  repository(owner:"lampepfl", name:"dotty") {
    |    object(expression: $commitSha){
    |      __typename
    |      ... on Commit {
    |        associatedPullRequests(first: 1) {
    |          nodes {
    |            number
    |            id
    |            mergedAt
    |          }
    |        }
    |      }
    |    }
    |  }
    |}
    """.send(
      uri"https://api.github.com/graphql",
      "DummyUser",
      apiToken
    )
  val pr = res.repository.`object`.asCommit.get.associatedPullRequests.nodes.head
  (ID(pr.id), pr.mergedAt)

def timestampItem(id: ID, date: String) =
  query"""
    |mutation editField {
    |  updateProjectV2ItemFieldValue(input: {
    |    projectId: $PROJECT_ID,
    |    itemId: $id,
    |    fieldId: $FIELD_ID,
    |    value: { text: $date }
    |  }) {
    |    projectV2Item {
    |      updatedAt
    |    }
    |  }
    |}
    """.send(
      uri"https://api.github.com/graphql",
      "DummyUser",
      apiToken
    )

def addItem(id: ID) =
  val res = query"""
    |mutation addItem {
    |  addProjectV2ItemById(input: {
    |    projectId: $PROJECT_ID,
    |    contentId: $id
    |  }) {
    |    item {
    |      id
    |    }
    |  }
    |}
    """.send(
      uri"https://api.github.com/graphql",
      "DummyUser",
      apiToken
    )
  ID(res.addProjectV2ItemById.item.id)
