# Module and infrastructure names

Apply these conventions to new resources. Preserve existing stack ownership,
logical IDs, physical names, endpoints and references.

- Modules: `modules/{name}`, with matching `moduleName`, without a project prefix.
- Per-environment stacks/resources: `whatsdone-{env}-{name}`.
- Shared stacks/resources: `whatsdone-{name}`.
- S3 bucket names must also be globally unique; check availability before creation.
- CloudFront origin IDs: `whatsdone-{name}`, with matching `TargetOriginId` references.
  These are distribution references, not CloudFormation resource logical IDs.
- CDK construct names: `Whatsdone` prefix.

Keep deployed domains. Include the environment in new non-production hostnames.
Review replacements, removals, aliases and name collisions before any rename.
