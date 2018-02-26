'use strict';

const _ = require('lodash');
const winston = require('winston');

// depending on your environment configuration, configure it properly here
const logger = new winston.Logger({
  transports: new winston.transports.Console({
    level: 'info', // default level, don't log any other below this
    json: process.env.LOG_JSON === 'true',
    timestamp: true,
    prettyPrint: process.env.LOG_JSON !== 'true'
  })
});

// the custom Logger class
module.exports = class MyLogger {
  // an array prop to keep track of all related logging data
  messages = [];

  // meta data
  metaData = {};

  constructor(metaData) {
    this.startedAt = Date.now();
    this.metaData = metaData || {};
  }

  /**
   * Append one new log entry into the mesage array
   *
   * @param {string} logLevel  error|warning|info|verbose
   *
   * @param {string} logTitle
   *
   * @param {any} message
   *
   */
  push(logLevel, logTitle, message) {
    if (!message) {
      message = '';
    }

    if (_.isObject(message)) {
      message = JSON.stringify(message, null, 2);
    }

    this.messages.push({ logLevel, logTitle, message });
  }

  /**
   * Write all the log data at once in an single log entry
   */
  write() {
    // log processing time if required
    this.logProcessingTime();

    // format the messages
    const messageStr = this.formatMessages();

    // detect the log level for combined log entry
    const logLevel = this.detectLogLevel();

    // metaData
    const metaData = this.metaData;

    // actually write the log entry
    logger[logLevel](messageStr, metaData);
  }

  // format the messages
  formatMessages() {
    const messages = this.messages;
    const formatEntry = (logEntry, idx) => {
      return `[${idx + 1}] ${logEntry.logLevel.toUpperCase()} ${logEntry.logTitle} : ${logEntry.message}`;
    };
    const messageStr = _.chain(messages).map(formatEntry).join('\n').value();

    return messageStr;
  }

  /**
   * detect the log level
   * will be error if there are any error in the messages list
   * if no error, will be warning if there are any warning in the messages list
   * otherwise, will be info
   * add more detector if you need
   *
   * @returns {string} error|warning|info
   */
  detectLogLevel() {
    const messages = this.messages;

    return _.find(messages, { logLevel: 'error' })
      ? 'error'
      : _.find(messages, { logLevel: 'warning' }) ? 'warning' : 'info';
  }

  // Add one extra entry for processing time
  logProcessingTime() {
    if (process.env.LOG_PROCESSING_TIME !== 'true') {
      return;
    }

    const currentTime = Date.now();
    const startedAt = this.startedAt;
    const processingTime = currentTime - startedAt;
    this.push('info', 'processingTime', `${processingTime} ms`);
  }
};
