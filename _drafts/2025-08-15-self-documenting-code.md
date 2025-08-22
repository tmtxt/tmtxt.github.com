---
layout: post
title: "Meaningful name"
description: ""
categories: [.net]
tags: []
thumbnail:
---

# If condition

```csharp
// comment here
if (!Parent.RegistrationYear.IsDefault && Math.Abs(yearCreateManifest - Parent.RegistrationYear) > 1)
			{
				var warningMessage = Res.GetString("6a5f9465-fa1a-4125-9c4d-328ec8a7ad49", "The Document Year cannot be more or less than 1 year from when the Manifest job got created");
				Parent.RegistrationYearInfo.AddWarning(warningMessage);
			}
```

# Functional Programming and LINQ Expression

fix

```csharp
			var result = Enumerable.Range(2, xlsFile.GetRowCount(1))
				.Select(CreateFromRow)
				.Where(FilterInvalidCode)
				.Where(FilterInvalidDescription)
				.DistinctBy(x => x.ZZD_Code);
			return result.ToList();
```