import * as path from "path";
import * as fs from 'fs'
import * as yaml from 'js-yaml'

// Read and parse the YAML file
const spagoYamlFilePath = path.resolve(__dirname, 'spago.yaml');
const spagoYamlFileContent = fs.readFileSync(spagoYamlFilePath, 'utf8');
const data = yaml.load(spagoYamlFileContent);

export default {
  base: `/${data.package.publish.location.githubRepo}/`,
  // root: './example',
  // build: {
  //   outDir: path.resolve(__dirname, "dist"),
  // },
  // mode: 'development',
  resolve: {
    alias: {
      PureScript: process.env.NODE_ENV === "production"
        ? path.resolve(__dirname, "output-es")
        : path.resolve(__dirname, "output"),
      // PureScript: path.resolve(__dirname, "output"),
    },
  }
};
