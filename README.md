## Prerequisities

You must first have [Node.js](https://nodejs.org) and [NPM](https://www.npmjs.com) installed. 

Then you can install [Bower](https://bower.io) using the command:

```
npm install -g bower
```

## Instructions

With these installed, you should be able to build the code with the command:

```
npm run build
```

This outputs the resulting code in `dist/example.js`, as well as libraries in the `output` directory. You should then be able to open `dist/index.js` in your web browser, and see the UI.

## To Run the "Simpler" Code

You can revert the latest "Routing and Navigation" commit, or you can go into `src/Main.purs` and change the `main` function to just call `runUI` on one of the components from another file. For instance, `Counter.purs`. Make sure to rebuild after you do this.
