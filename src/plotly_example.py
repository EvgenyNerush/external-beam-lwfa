import plotly.express as px

xs = range(10)
ys = [x * x for x in xs]
zs = [x / (5 + x) for x in xs]

# see px.scatter? in ipython
fig = px.scatter(x = xs, y = ys, color = zs)

fig.update_layout(coloraxis_colorbar=dict(title="qwe"))

fig.write_json('plotly_example.json')
fig.show()
