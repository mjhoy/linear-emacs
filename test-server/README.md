# Linear Mock GraphQL Server

A minimal Rust server that mocks Linear's GraphQL API for testing purposes.

## Usage

```bash
# Start the server
cargo run

# Server runs on http://localhost:8080
# GraphQL endpoint: http://localhost:8080/graphql
```

## Supported Queries

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
  issue(id: "test-issue-1") {
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

## Test Data

The server serves fixture data from the `fixtures/` directory:

- `assigned-issues.json` - List of assigned issues
- `issue-test-issue-1.json` - Detailed view for TEST-1
- `issue-test-issue-2.json` - Detailed view for TEST-2 (null description)
- `issue-test-issue-3.json` - Detailed view for TEST-3