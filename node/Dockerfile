FROM node:boron

# Create app directory
WORKDIR /src
ADD . /src

# Install app dependencies
COPY package.json .
# For npm@5 or later, copy package-lock.json as well
#COPY package.json package-lock.json ./

RUN npm install --no-optional -g nodemon

# Provides cached layer for node_modules
ADD package.json /tmp/package.json
RUN cd /tmp && npm install
RUN mkdir -p /src && cp -a /tmp/node_modules /src/

# Bundle app source
COPY . .

EXPOSE 8080
CMD [ "nodemon", "/src/app.js" ]

