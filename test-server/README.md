# linear mock graphql server

A minimal Rust server that mocks Linear's GraphQL API for testing purposes.

## Usage

```bash
# Start the server
cargo run

# GraphQL endpoint at http://localhost:8080/graphql
```

## Queries

### Get assigned issues
```graphql
{
  viewer {
    assignedIssues {
      nodes {
        id
        identifier
        title
        url
        state {
          name
          color
        }
      }
    }
  }
}
```

### Get issue details
```graphql
{
  issue(id: "92ddcc64-5765-4a41-980e-49493eb086b3") {
    id
    identifier
    title
    url
    description
    state {
      name
      color
    }
    assignee {
      name
      email
    }
    labels {
      nodes {
        name
        color
      }
    }
    comments {
      nodes {
        body
        user {
          name
        }
        createdAt
      }
    }
  }
}
```
