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
//     projectV2(number: 2) { id }  // 3.3 LTS
//     projectV2(number: 9) { id }  // 3.9 LTS
//   }
// }
val PROJECT33_ID = ID("PVT_kwDN3uPOAHewkg")
val PROJECT39_ID = ID("PVT_kwDN3uPOAYfwdg")

// Obtained with:
// query {
//   organization(login: "scala") {
//     projectV2(number: N) {
//       field(name: "Merged at") {
//         ... on ProjectV2FieldCommon { id }
//       }
//     }
//   }
// }
val FIELD33_ID = ID("PVTF_lADN3uPOAHewks4E3B1I")
val FIELD39_ID = ID("PVTF_lADN3uPOAYfwds4YOKnv")

val PROJECTS = List(
  (PROJECT33_ID, FIELD33_ID),
  (PROJECT39_ID, FIELD39_ID)
)

@main def run(commitSha: String) =
  val (id, date) = getPrData(commitSha)
  for (projectId, fieldId) <- PROJECTS do
    val newId = addItem(projectId, id)
    timestampItem(projectId, fieldId, newId, date)

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

def timestampItem(projectId: ID, fieldId: ID, id: ID, date: String) =
  query"""
    |mutation editField {
    |  updateProjectV2ItemFieldValue(input: {
    |    projectId: $projectId,
    |    itemId: $id,
    |    fieldId: $fieldId,
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

def addItem(projectId: ID, id: ID) =
  val res = query"""
    |mutation addItem {
    |  addProjectV2ItemById(input: {
    |    projectId: $projectId,
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
