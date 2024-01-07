---
layout: post
title: "Running Docker Container with GCP Compute Engine"
description: ""
categories: [misc]
tags: []
thumbnail:
---

# About the why

You may wonder why we have to run container inside Google Compute Engine while GCP has already
offered several fully managed container platforms like Cloud Function, Cloud Run or Google-managed
Kubernetes cluster. Well, each of them has its own advantages and drawbacks. Cloud Function and
Cloud Run are not designed for long running task, where a process handles many things in background
or wait for the signal from non-http request source. Kubernetes, could be used
for multiple purposes. However, k8s is usually too overkill for simple application. Even for a
matured one, if you don't follow Microservices architecture or your team size is not that big, very
likely that you even need k8s. Managing a k8s cluster is never an easy task, even for Google-managed
version.

GCP Compute Engine (GCE), could be considered something in the middle. It offers the flexibility of
a fully-accessed server while still help you auto scale the services in some manner. You may also
want to run Docker container on that GCE instance instead of launching the application directly due
to the benefits of Docker container. I'm going to summarize some ...

# GCP Service Account

You need to create a Service account and bind all the roles that you want to grant to that VM
instance. We will then assign this Service account to the VM instance. The container running inside
that VM instance will have the same set of permissions as the VM.

[IAM basic and predefined roles reference](https://cloud.google.com/iam/docs/understanding-roles)

```typescript
import * as gcp from '@pulumi/gcp';

// create the service account
const serviceAccount = new gcp.serviceaccount.Account(
  `my-app-service-account`,
  {
    accountId: 'my-app-service-account',
    displayName: 'My App Service Account',
  }
);

// bind all the roles
new gcp.projects.IAMMember(`my-all-iam-binding-object`, {
  role: 'roles/storage.objectViewer',
  member: pulumi.interpolate`serviceAccount:${serviceAccount.email}`,
  project: 'my-project',
});
new gcp.projects.IAMMember(`my-all-iam-binding-pubsub`, {
  role: 'roles/pubsub.publisher',
  member: pulumi.interpolate`serviceAccount:${serviceAccount.email}`,
  project: 'my-project',
});
// other roles if necessary
```

# GCP Instance Template

You need to specify how the instance will be created. We will use the Container-Optimzied OS VM
Image from Google, which is based on CoreOS. For this VM Image, you can simply declare the Container
specs in the instance metadata and it will run that container automatically after starting the VM.

To start with, install `yaml` package to stringify the metadata object
```
npm install --save yaml
```

and then define the instance template

```typescript
const env = [
  { name: 'DATABASE_HOST', value: 'db-server.com' },
  { name: 'DATABASE_PORT', value: '5432' }
];

const containerSpecs = {
  containerDeclaration: {
    spec: {
      containers: [
        {
          name: 'my-app',  // container name
          image: 'my-app:v1.0.0',
          env: env,
          stdin: false,
          tty: false,
        },
      ],
      restartPolicy: 'Always',
    },
  },
};
const metadata = {
  'gce-container-declaration': yaml.stringify(containerSpecs)
}

const instanceTemplate = new gcp.compute.InstanceTemplate(
  `my-app-instance-template`,
  {
    // select your machine specs, for example https://cloud.google.com/compute/docs/general-purpose-machines
    machineType: 'c3d-standard-4',
    metadata,
    disks: [
      // in the future, there may be newer version
      { sourceImage: 'projects/cos-cloud/global/images/family/cos-109-lts' },
    ],
    serviceAccount: {
      email: serviceAccount.email,
      scopes: ['cloud-platform'],
    }
  }
);
```

# GCP Instance Group Manager

