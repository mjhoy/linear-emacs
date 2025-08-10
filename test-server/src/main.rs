use async_graphql::{
    http::GraphiQLSource, EmptyMutation, EmptySubscription, InputObject, Object, Schema,
    SimpleObject,
};
use async_graphql_axum::GraphQL;
use axum::{
    response::{self, IntoResponse},
    routing::get,
    Router,
};
use serde::Deserialize;
use std::fs;

#[derive(SimpleObject, Deserialize)]
struct Issue {
    id: String,
    identifier: String,
    title: String,
    url: String,
    description: Option<String>,
    state: State,
    assignee: Option<User>,
    labels: LabelConnection,
    comments: CommentConnection,
}

#[derive(SimpleObject, Deserialize)]
struct State {
    name: String,
    color: String,
}

#[derive(SimpleObject, Deserialize)]
struct User {
    name: String,
    email: Option<String>,
}

#[derive(SimpleObject, Deserialize)]
struct Label {
    name: String,
    color: String,
}

#[derive(SimpleObject, Deserialize)]
struct Comment {
    body: String,
    user: User,
    #[graphql(name = "createdAt")]
    #[serde(rename = "createdAt")]
    created_at: String,
}

#[derive(SimpleObject, Deserialize)]
struct LabelConnection {
    nodes: Vec<Label>,
}

#[derive(SimpleObject, Deserialize)]
struct CommentConnection {
    nodes: Vec<Comment>,
}

#[derive(SimpleObject)]
struct IssueConnection {
    nodes: Vec<Issue>,
}

#[derive(InputObject)]
struct NullFilter {
    null: bool,
}

#[derive(InputObject)]
struct IssueFilter {
    #[graphql(name = "completedAt")]
    completed_at: Option<NullFilter>,
    #[graphql(name = "canceledAt")]
    canceled_at: Option<NullFilter>,
}

struct Viewer;

fn load_issues() -> Vec<Issue> {
    (1..=3)
        .map(|index| {
            let data = fs::read_to_string(format!("fixtures/issue-test-issue-{index}.json"))
                .expect("Failed to read issue json");
            let json: serde_json::Value =
                serde_json::from_str(&data).expect("Failed to parse issue json");
            let issue_data = json["data"]["issue"].clone();
            let issue: Issue = serde_json::from_value(issue_data).expect("Failed to parse issue");

            return issue;
        })
        .collect::<Vec<_>>()
}

struct Query;

#[Object]
impl Viewer {
    #[graphql(name = "assignedIssues")]
    async fn assigned_issues(&self, filter: Option<IssueFilter>) -> IssueConnection {
        let issues: Vec<Issue> = load_issues();

        // For simplicity, we ignore the filter and return all issues
        // In a real implementation, you'd filter based on the criteria
        IssueConnection { nodes: issues }
    }
}

#[Object]
impl Query {
    async fn viewer(&self) -> Viewer {
        Viewer
    }

    async fn issue(&self, id: String) -> Option<Issue> {
        let issues: Vec<Issue> = load_issues();

        issues.into_iter().find(|issue| issue.id == id)
    }
}

async fn graphiql() -> impl IntoResponse {
    response::Html(GraphiQLSource::build().endpoint("/graphql").finish())
}

#[tokio::main]
async fn main() {
    let schema = Schema::build(Query, EmptyMutation, EmptySubscription).finish();

    let app = Router::new().route("/graphql", get(graphiql).post_service(GraphQL::new(schema)));

    println!("GraphQL Playground: http://localhost:8080/graphql");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
