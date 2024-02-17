FROM ruby:3.3

RUN apt-get install -y curl \
  && curl -sL https://deb.nodesource.com/setup_18.x | bash - \
  && apt-get install -y nodejs \
  && curl -L https://www.npmjs.com/install.sh | sh

RUN gem update --system 3.5.6
RUN gem install bundler:2.5.6

WORKDIR /srv/jekyll

COPY Gemfile .
COPY Gemfile.lock .


RUN echo -n "bundle version: " && bundle --version
RUN bundle install
RUN mkdir /opt/npm-global
RUN npm config set prefix '/opt/npm-global'
RUN npm config set global true
RUN npm install  bower
RUN echo -n "npm version: " && npm --version
RUN chmod u+s /bin/chown
RUN date
